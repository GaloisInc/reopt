{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import qualified Control.Concurrent.Async as ASync
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad (forM_)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ElfEdit as Elf
import           Data.Foldable
import qualified Data.HashMap.Strict as HMap
import           Data.IORef
import           Data.LLVM.BitCode
import           Data.List as List
import           Data.Macaw.CFG
import           Data.Macaw.Memory.ElfLoader
import qualified Data.Macaw.Types as M
import           Data.Macaw.X86
import           Data.Macaw.X86.X86Reg
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as LText
import           Data.Typeable
import           Data.Word
import qualified Data.Yaml as Yaml
import           GHC.Natural
import           Numeric
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import qualified Text.LLVM as L
import           Text.LLVM hiding (comment, (:>), Value, LayoutSpec(..))
import qualified Text.LLVM.PP as L
import           Text.Printf
import qualified What4.Protocol.SMTLib2.Parse as SMTP
import qualified What4.Protocol.SMTLib2.Syntax as SMT

import qualified Reopt.VCG.Annotations as Ann
import           VCGCommon
import qualified VCGMacaw as M

$(pure [])

-- | Pretty print a block label for display purposes.
ppBlock :: BlockLabel -> String
ppBlock (Named (Ident s)) = s
ppBlock (Anon i) = show i

$(pure [])

-- | Find a block with the given label in the config.
findBlock :: Ann.FunctionAnn -> BlockLabel -> Maybe Ann.BlockAnn
findBlock cfg lbl = HMap.lookup (ppBlock lbl) (Ann.blocks cfg)

$(pure [])

comment :: Builder -> SMT.Command
comment r = SMT.Cmd $ "; " <> r

$(pure [])

------------------------------------------------------------------------
-- ModuleVCG

-- | Abstract interface to SMT solver.
data ProverInterface = ProverInterface
  { addCommandCallback :: !(SMT.Command -> IO ())
    -- ^ Invoked to add an SMT command.
    --
    -- These commands must not change the solver out of assert mode.
  , proveFalseCallback :: !(SMT.Term -> String -> IO ())
    -- ^ Invoked when we have a proposition to prove is false for all
    -- interprettions.
    --
    -- The message is provide so the user knows the source of the
    -- check.
  , proveTrueCallback  :: !(SMT.Term -> String -> IO ())
    -- ^ Invoked when we have a proposition to prove is true for all
    -- interpretations.
    --
    -- The message is provide so the user knows the source of the
    -- check.
  }

$(pure [])

type FunctionName = String

-- | Function for creating sessions to interact with SMT solver.
--
-- As blocks can be independently verified, we create a separate
-- prover interface for each block to be verified.
data ProverSessionGenerator
   = PSGen { blockCallbacks :: forall a . FunctionName -> String -> (ProverInterface -> IO a) -> IO a
           , sessionComplete :: IO ()
           }

$(pure [])

------------------------------------------------------------------------
-- ModuleVCG

-- | Information needed for checking equivalence of entire module
data ModuleVCGContext =
  ModuleVCGContext { moduleMem :: !(Memory 64)
                     -- ^ Machine code memory
                   , symbolAddrMap :: !(Map BS.ByteString (MemSegmentOff 64))
                     -- ^ Maps bytes to the symbol name
                   , writeStderr :: !Bool
                     -- ^ Controls whether logs, warnings or errors
                     -- chould be written to stderr.
                     --
                     -- If false, the messages are droped, but warning
                     -- count is increased.
                   , errorCount :: !(IORef Natural)
                     -- ^ Counts numbers of warnings generated during
                     -- verification for display at end of run.
                   , proverGen :: !ProverSessionGenerator
                     -- ^ Interface for generating prover sessions.
                   }

$(pure [])

newtype ModuleError = ModuleError String
  deriving (Typeable, Show)

instance Exception ModuleError where
  displayException (ModuleError msg) = msg

-- | A monad for running verification of an entire module
newtype ModuleVCG a = ModuleVCG { _unModuleVCG :: ReaderT ModuleVCGContext IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader ModuleVCGContext
           )

runModuleVCG :: ModuleVCGContext -> ModuleVCG () -> IO ()
runModuleVCG ctx (ModuleVCG m) = do
  catch (runReaderT m ctx) $ \(ModuleError e) -> do
    error $ "Uncaught module VCG error: " ++ e

vcgLog :: String -> ModuleVCG ()
vcgLog msg = do
  ctx <- ask
  when (writeStderr ctx) $ do
    liftIO $ hPutStrLn stderr msg

{-
moduleWarn :: String -> ModuleVCG ()
moduleWarn msg = do
  vcgLog $ "Error: " ++ msg
  ctx <- ask
  liftIO $ modifyIORef' (errorCount ctx) (+1)
-}

-- | A warning that stops execution until catch.
moduleThrow :: String -> ModuleVCG a
moduleThrow msg = do
  liftIO $ throw (ModuleError msg)

-- | Catch a VCG error
moduleCatch :: ModuleVCG () -> ModuleVCG ()
moduleCatch (ModuleVCG m) = ModuleVCG $ ReaderT $ \ctx -> do
  catch (runReaderT m ctx) $ \(ModuleError e) -> do
    when (writeStderr ctx) $ do
      hPutStrLn stderr $ "Error: " ++ e
    modifyIORef' (errorCount ctx) (+1)

$(pure [])

------------------------------------------------------------------------
-- BlockVCG


-- | Information that does not change during execution of @BlockVCG@.
data BlockVCGContext = BlockVCGContext
  { mcModuleVCGContext :: !ModuleVCGContext
    -- ^ Information about machine code module.
  , curFunAnnotations :: !Ann.FunctionAnn
    -- ^ Annotations for the current function we are verifying.
  , firstBlockLabel :: !BlockLabel
    -- ^ Label for first block in this function
  , callbackFns :: !ProverInterface
    -- ^ Functions for interacting with SMT solver.
  , mcBlockEndAddr :: !(MemAddr 64)
    -- ^ The end address of the block.
  , mcBlockMap :: !(Map (MemSegmentOff 64) Ann.BlockEventInfo)
    -- ^ Map from addresses to annotations of events on that address.
  }

$(pure [])

-- | State that changes during execution of @BlockVCG@.
data BlockVCGState = BlockVCGState
  { mcCurAddr :: !(MemSegmentOff 64)
    -- ^ Address of the current instruction
  , mcCurSize :: !(MemWord 64)
    -- ^ Size of current instruction.
  , mcX87Top :: !Int
    -- ^ Top index in x86 stack (starts at 7 and grows down).
  , mcDF :: !Bool
    -- ^ Direction flag
  , mcCurRegs :: !(RegState X86Reg (Const SMT.Term))
    -- ^ Map registers to the SMT term.
  , mcMemIndex :: !Natural
    -- ^ Index to give to next memory index
  , mcUsedAllocas :: !(Set Ann.AllocaName)
    -- ^ Map names used for allocations to @(b,e)@ where @[b,e)@
    -- represents the hardware range for addresses in the binary.
  , mcEvents :: ![M.Event]
    -- ^ Unprocessed events from last instruction.
  , mcLocalIndex :: !Integer
    -- ^ Index of next local variable for machine code.
  , mcPendingAllocaOffsetMap :: !(Map Ann.AllocaName Ann.AllocaInfo)
    -- ^ This is a map from allocation names to the offset relative to
    -- the top of the function stack frame.  In this case, we do not
    -- include the 8 bytes storing the return address.  These are
    -- allocas that have not been made when the block starts.
  }

$(pure [])

-- | A Monad for verifying an individual block.
newtype BlockVCG a = BlockVCG { unBlockVCG :: BlockVCGContext
                                           -> (a -> BlockVCGState -> IO ())
                                           -> BlockVCGState
                                           -> IO () }

$(pure [])

instance Functor BlockVCG where
  fmap f (BlockVCG g) = BlockVCG (\ctx c -> g ctx (c . f))

instance Applicative BlockVCG where
  pure x = seq x $ BlockVCG $ \_ c s -> c x s
  BlockVCG mf <*> BlockVCG mx = BlockVCG $ \ctx c s0 -> do
    mf ctx (\f s1 -> mx ctx (\x s2 -> let y = f x in seq y $ c y s2) s1) s0

instance Monad BlockVCG where
  BlockVCG m >>= h = BlockVCG $ \ctx c s0 -> m ctx (\a s1 -> unBlockVCG (h a) ctx c s1) s0

instance MonadReader BlockVCGContext BlockVCG where
  ask = BlockVCG $ \ctx c s -> c ctx s
  local f (BlockVCG m) =
    BlockVCG $ \ctx c s ->
                 let ctx' = f ctx
                  in seq ctx' $ m ctx' c s

instance MonadState BlockVCGState BlockVCG where
  get = BlockVCG $ \_ c s -> c s s
  put t = seq t $ BlockVCG $ \_ c _ -> c () t

instance MonadIO BlockVCG where
  liftIO m = BlockVCG $ \_ctx c s -> m >>= \a -> c a s

$(pure [])

-- | Stop verifying this block.
haltBlock :: BlockVCG a
haltBlock = BlockVCG $ \_ _ _ -> pure ()

-- | Report an error at the given location and stop verification of this block.
errorAt :: String -> BlockVCG a
errorAt msg = do
  addr <- gets $ mcCurAddr
  liftIO $ hPutStrLn stderr $ "At " ++ showsPrec 10 addr ": " ++ msg
  haltBlock

$(pure [])

addCommand :: SMT.Command -> BlockVCG ()
addCommand cmd = do
  prover <- asks callbackFns
  liftIO $ addCommandCallback prover cmd

$(pure [])

-- | @proveFalse p msg@ adds a proof obligation @p@ is false for all
-- interpretations of constants with the message @msg@.
proveFalse :: SMT.Term -> String -> BlockVCG ()
proveFalse p msg = do
  fns <- asks callbackFns
  liftIO $ proveFalseCallback fns p msg

-- | @proveTrue p msg@ adds a proof obligation @p@ is true for all
-- interpretations of constants with the message @msg@.
proveTrue :: SMT.Term -> String -> BlockVCG ()
proveTrue p msg = do
  fns <- asks callbackFns
  liftIO $ proveTrueCallback fns p msg

-- | @proveEq x y msg@ add a proof obligation named @msg@ asserting
-- that @x@ equals @y@.
proveEq :: SMT.Term -> SMT.Term -> String -> BlockVCG ()
proveEq x y msg = proveFalse (SMT.distinct [x,y]) msg

-- | Assume the propositon is true without requiring it to be proven.
assume :: SMT.Term -> BlockVCG ()
assume p = addCommand $ SMT.assert p

$(pure [])

-- | This records the current LLVM statement for pretty-printing purposes
setCurrentLLVMStmt :: L.Stmt -> BlockVCG ()
setCurrentLLVMStmt stmt = do
  addCommand $ comment ("LLVM: " <> fromString (show (L.ppLLVM38 (L.ppStmt stmt))))

$(pure [])

-- | Use a map from symbol names to address to find address.
getMCAddrOfLLVMFunction :: Map BS.ByteString (MemSegmentOff 64)
                           -- ^ Map from symbol names in machine code
                           -- to the address in the binary.
                        -> String
                        -> Either String (MemWord 64)
getMCAddrOfLLVMFunction m nm = do
  let llvmFun = UTF8.fromString nm
  case Map.lookup llvmFun m of
    Nothing -> Left $ "Cannot find address of LLVM symbol: " ++ nm
    Just expectedAddr ->
      case segoffAsAbsoluteAddr expectedAddr of
        Just addr -> pure addr
        Nothing -> Left $ "Could not resolve concrete address for " ++ nm

-- | Denotes the "name" of an address for pretty printing purposes.
type AddrName = Text

-- | Return the name for SMTLIB display purposes of an address.
addrName :: MemWidth w => MemAddr w -> AddrName
addrName addr
    | addrBase addr == 0 = Text.pack $ 'a' : off ""
    | otherwise          = Text.pack $ 'r' : (base . showChar '_' . off) ""
  where base = shows (addrBase addr)
        off  = showHex (addrOffset addr)

-- | Return the name of the SMT variable for the register at the
-- given PC.
addrStartRegValue :: AddrName -> X86Reg tp -> Text
addrStartRegValue b reg = b <> "_" <> Text.pack (show reg)

-- | Declare an SMT value for each register that defines the value of
-- the register when the function at the given address starts
-- execution.
declareAddrStartRegValues :: ProverInterface
                          -> AddrName
                          -> [(Some X86Reg, SMT.Term)]
                          -> IO (RegState X86Reg (Const SMT.Term))
declareAddrStartRegValues prover nm definedRegs = do
  let definedRegMap = Map.fromList definedRegs
  let initReg :: X86Reg tp -> IO (Const SMT.Term tp)
      initReg reg = do
        let regName = addrStartRegValue nm reg
        case Map.lookup (Some reg) definedRegMap of
          Nothing -> do
            addCommandCallback prover $! SMT.declareFun regName  [] (M.toSMTType (M.typeRepr reg))
          Just t -> do
            addCommandCallback prover $! SMT.defineFun regName  [] (M.toSMTType (M.typeRepr reg)) t
        pure $! Const (varTerm regName)
  mkRegStateM initReg

-- | This is the list of callee saved registers.
calleeSavedGPRegs :: [X86Reg (M.BVType 64)]
calleeSavedGPRegs = X86_GP <$> Ann.calleeSavedGPRegs

-- | Return the name of the SMT variable for the register when the
-- function starts.
--
-- This is used by callee saved registers
functionStartRegValue :: X86Reg tp -> Text
functionStartRegValue reg = "fnstart_" <> Text.pack (show reg)

-- | Declare an SMT value for each register that defines the value of the
-- register when the block starts execution.
declareFunctionStartRegValues :: ProverInterface -> IO ()
declareFunctionStartRegValues prover = do
  let initReg reg =
        addCommandCallback prover $!
          SMT.declareFun (functionStartRegValue reg) [] (M.toSMTType (M.typeRepr reg))
  traverse_ initReg calleeSavedGPRegs

-- | Assert that the functions identified by the LLVM and macaw
-- function pointers are equivalent.
assertFnNameEq :: L.Symbol -> SMT.Term -> BlockVCG ()
assertFnNameEq (L.Symbol nm) macawIP = do
  -- Get the map from symbol names to the associated address
  addrMap <- asks $ symbolAddrMap . mcModuleVCGContext
  -- Get the address in the original binary of the executable.
  let Right addr = getMCAddrOfLLVMFunction addrMap nm
  -- Generate an SMT term with the address associated with the symbol.
  let expectedAddrTerm = SMT.bvhexadecimal (toInteger addr) 64
  -- Assert the two addresses are equal.
  proveEq expectedAddrTerm macawIP ("Equivalence of function: " ++ nm)

x86ArgRegs :: [X86Reg (M.BVType 64)]
x86ArgRegs = [ RDI, RSI, RDX, RCX, R8, R9 ]

-- | Information about a supported memory type.
data SupportedMemType = SupportedMemType { supportedSuffix :: !String
                                         , supportedSort :: !SMT.Sort
                                         , supportedReadDecl  :: !SMT.Command
                                         , supportedWriteDecl :: !SMT.Command
                                         }

-- | A pair containing a memrepr and the operations needed to support it in VCG.
type SupportedMemPair = (Some MemRepr, SupportedMemType)


$(pure [])

-- | Read a number of bytes as a bitvector.
-- Note. This refers repeatedly to ptr so, it should be a constant.
readBVLE :: SMT.Term -- ^ Memory
         -> SMT.Term  -- ^ Address to read
         -> Natural -- ^ Number of bytes to read.
         -> SMT.Term
readBVLE mem ptr0 w = go (w-1)
  where go :: Natural -> SMT.Term
        go 0 = SMT.select mem ptr0
        go i =
          let ptr = SMT.bvadd ptr0 [SMT.bvdecimal (toInteger i) 64]
           in SMT.concat (SMT.select mem ptr) (go (i-1))

memVar :: Natural -> Text
memVar i = "x86mem_" <> Text.pack (show i)

-- | Create mem write declaration given number of bytes to write
memReadDeclBV :: Natural
              -> SMT.Command
memReadDeclBV w = do
  let nm = "mem_readbv" <> fromString (show (8*w))
  let args = [("m", memSort), ("a", ptrSort)]
  let resSort = SMT.bvSort (8*w)
  SMT.defineFun nm args resSort (readBVLE (varTerm "m") (varTerm "a") w)

$(pure [])

-- | @readFromMCMemory mem addr tp@ reads a value with type @tp@ from
-- address @addr@ in machine code memory @mem@.
--
-- This is a total function, so if you read from invalid memory, it
-- denotes some value, so you need to check whether the address is
-- valid independently.
readFromMCMemory :: SMT.Term -> SMT.Term -> SupportedMemType -> SMT.Term
readFromMCMemory mem addr supType =
  SMT.term_app ("mem_read" <> fromString (supportedSuffix supType)) [mem, addr]

-- | @defineVarWithReadValue var addr tp@ reads a value with type @tp@
-- from the address and assigns the value to @var@.
--
-- Note. This function assumes that code has already checked the read is value.
defineVarWithReadValue :: Var -> SMT.Term -> SupportedMemType -> BlockVCG ()
defineVarWithReadValue valVar addr supType = do
  mem <- gets  $ varTerm . memVar . mcMemIndex
  addCommand $ SMT.defineFun valVar [] (supportedSort supType) (readFromMCMemory mem addr supType)

$(pure [])

-- | Create mem write declaration given number of bytes to write
memWriteDeclBV :: Natural -> SMT.Command
memWriteDeclBV w = do
  let nm = "mem_writebv" <> fromString (show (8*w))
  let argTypes = [("m", memSort), ("a", ptrSort), ("v", SMT.bvSort (8*w))]
  SMT.defineFun nm argTypes memSort (writeBVLE (varTerm "m") (varTerm "a") (varTerm "v") w)

$(pure [])


-- | @supportedBVMemType byteCount@ constructs the @SupportedMemType@ structure
-- that reads a bitvector with @byteCount@ bytes.
supportedBVMemType :: Natural -> SupportedMemType
supportedBVMemType byteCount = seq byteCount $
  SupportedMemType { supportedSuffix    = "bv" <> show (8*byteCount)
                   , supportedSort      = SMT.bvSort (8*byteCount)
                   , supportedReadDecl  = memReadDeclBV  byteCount
                   , supportedWriteDecl = memWriteDeclBV byteCount
                   }

-- | Construct a known mem pair for a nat.
supportedBVMemPair :: (1 <= w) => NatRepr w -> SupportedMemPair
supportedBVMemPair w =
  ( Some (BVMemRepr w LittleEndian)
  , supportedBVMemType (natValue w)
  )

$(pure [])

addrSupportedMemType :: SupportedMemType
addrSupportedMemType = supportedBVMemType 8

-- | Types that may appear in reads/writes.
supportedMemTypes :: Map (Some MemRepr) SupportedMemType
supportedMemTypes = Map.fromList $
  [ supportedBVMemPair (knownNat @1)
  , supportedBVMemPair (knownNat @2)
  , supportedBVMemPair (knownNat @4)
  , supportedBVMemPair (knownNat @8)
  ]

$(pure [])

getSupportedType :: MemRepr tp -> BlockVCG SupportedMemType
getSupportedType memType =
  case Map.lookup (Some memType) supportedMemTypes of
    Nothing -> error $ "Unexpected type " ++ show memType
    Just supType -> pure supType

$(pure [])

-- | @macawWrite addr cnt val@ writes @cnt@ bytes to memory.
--
-- The value written is the @8*cnt@-length bitvector term @val@.
macawWrite :: SMT.Term -> MemRepr tp -> SMT.Term -> BlockVCG ()
macawWrite addr memType val = do
  supType <- getSupportedType memType
  idx <- gets mcMemIndex
  modify' $ \s -> s { mcMemIndex = mcMemIndex s + 1 }
  let mem = varTerm (memVar idx)
  let suf = supportedSuffix supType
  let newMem = SMT.term_app ("mem_write" <> fromString suf) [mem, addr, val]
  addCommand $ SMT.defineFun (memVar (idx+1)) [] memSort newMem

$(pure [])

-- | Name of SMT predicate that holds if all the bytes [addr, addr+sz)` are
-- in a region of the stack frame marked as only accessible to the binary code.
--
-- Note. The correctness property above assumes that @sz > 0@.
onMCFunStack :: IsString a => a
onMCFunStack = "on_stack_frame"

addc :: SMT.Term -> Natural -> SMT.Term
addc t 0 = t
addc t i = SMT.bvadd t [SMT.bvdecimal (toInteger i) 64]

subc :: SMT.Term -> Natural -> SMT.Term
subc t 0 = t
subc t i = SMT.bvsub t (SMT.bvdecimal (toInteger i) 64)

-- | @defineRangeCheck nm low high@ introduces the definition for a
-- function named @nm@ that takes an address @a@ and size @sz@, and
-- checks that @[a..a+sz)@ is in @[low..high)@.
defineRangeCheck :: Text -> SMT.Term -> SMT.Term -> SMT.Command
defineRangeCheck nm low high = do
  let args = [("a", ptrSort), ("sz", SMT.bvSort 64)]
  SMT.defineFun nm args SMT.boolSort $
    SMT.letBinder [ ("e", SMT.bvadd (varTerm "a") [varTerm "sz"]) ] $
      SMT.and [ SMT.bvule low (varTerm "a")
              , SMT.bvule (varTerm "a") (varTerm "e")
              , SMT.bvule (varTerm "e") high
              ]

evalRangeCheck :: Text -> SMT.Term -> Integer -> SMT.Term
evalRangeCheck nm a sz = SMT.term_app (Builder.fromText nm) [a, SMT.bvdecimal sz 64]


allocaMCBaseEndDecls :: Ann.AllocaInfo -> [SMT.Command]
allocaMCBaseEndDecls a =
  let nm = Ann.allocaName a
      base = Ann.allocaBinaryOffset a
      end  = base - Ann.allocaSize a
  -- Define machine code base for allocation.
   in [ SMT.defineFun (allocaMCBaseVar nm) [] ptrSort (subc (varTerm "stack_high") base)
      , SMT.defineFun (allocaMCEndVar  nm) [] ptrSort (subc (varTerm "stack_high") end)
        -- Introduce predicate to check machine-code addresses.
      , defineRangeCheck (isInMCAlloca nm) (varTerm (allocaMCBaseVar nm)) (varTerm (allocaMCEndVar nm))
      ]

-- | @mcMemDecls byetsAbove@ adds declarations about the memory.
--
-- It assumes that there is a fresh constant blockinit_RSP declared for
-- the initial RSP, and asserts that @sz < blockinit_RSP < 2^64 - 8@
--
-- It defines @stack_low@ to be @blockinit_RSP - sz@.
-- It defines @stack_high@ to be @blockinit_RSP@.
--
-- It also defines @heap_low@, @heap_high@, and @in_heap_segment@.
--
-- It defines @on_this_stack_frame@
-- Note. This assumes X86 registers are already declared.
mcMemDecls :: Natural
              -- ^ The size of the stack frame for this function
              -- not including the return address for function
           -> [Ann.AllocaInfo]
              -- ^ Allocations
           -> [SMT.Command]
mcMemDecls fnSize allocas
  = [ SMT.declareConst "heap_low" (SMT.bvSort 64)
    , SMT.declareConst "heap_high" (SMT.bvSort 64)
    , SMT.assert $ SMT.bvult (varTerm "heap_low") (varTerm "heap_high")
      -- Declare in_heap_segment
    , defineRangeCheck "in_heap_segment" (varTerm "heap_low") (varTerm "heap_high")
    , comment "stack_high is the maxium address on stack in this frame."
    , comment "This points to the address the return address is stored at."
    , SMT.declareFun "stack_high"  [] (SMT.bvSort 64)
    , SMT.assert $ SMT.bvult (varTerm "stack_high") (SMT.bvhexadecimal (2^(64::Int) - 8) 64)
    , SMT.assert $ SMT.bvugt (varTerm "stack_high") (SMT.bvdecimal (toInteger fnSize) 64)
      -- High water stack pointer includes 8 bytes for return address.
      -- The return address top must be aligned to a 16-byte boundary.
      -- This is done by asserting the 4 low-order bits of stack_high are 8.
    , SMT.assert $ SMT.eq [ SMT.extract 3 0 (varTerm "stack_high"), SMT.bvdecimal 8 4]
    , comment "stack_low is the minimum address on stack in this frame."
    , SMT.defineFun "stack_low"  [] (SMT.bvSort 64) (subc (varTerm "stack_high") fnSize)
    ]
  ++ concatMap allocaMCBaseEndDecls allocas
  -- Declare on_this_stack_frame
  ++ [ let args = [("a", ptrSort), ("sz", SMT.bvSort 64)]
           inStack =
             [ SMT.bvule (varTerm "stack_low") (varTerm "a")
             , SMT.bvule (varTerm "a") (varTerm "e")
             , SMT.bvule (varTerm "e") (addc (varTerm "stack_high") 8)
             ]
           disjointFn a =
             let nm = Ann.allocaName a
              in isDisjoint (varTerm "a", varTerm "e")
                            (varTerm (allocaMCBaseVar nm), varTerm (allocaMCEndVar nm))
        in SMT.defineFun onMCFunStack args SMT.boolSort $
             SMT.letBinder [ ("e", SMT.bvadd (varTerm "a") [varTerm "sz"]) ] $
               SMT.and (inStack ++ fmap disjointFn allocas)
     ]

-- | A SMT predicate that holds if all the bytes [addr, addr+sz)` is in the heap.
--
-- Note. This predicate can assume that `sz > 0` and `sz < 2^64`, but still
-- be correct if the computation of `addr+sz` overflows.
inHeapSegment :: SMT.Term -> Integer -> SMT.Term
inHeapSegment addr sz = SMT.term_app "in_heap_segment" [addr, SMT.bvdecimal sz 64]

------------------------------------------------------------------------

-- | When that a feature is missing.
missingFeature :: String -> BlockVCG ()
missingFeature msg = liftIO $ hPutStrLn stderr $ "TODO: " ++ msg

-- | Identifies the LLVM base address of an allocation.
allocaLLVMBaseVar :: Ann.AllocaName -> Text
allocaLLVMBaseVar (Ann.AllocaName nm) = mconcat ["alloca_", nm, "_llvm_base"]

-- | Identifies the LLVM end address of an allocation.
allocaLLVMEndVar :: Ann.AllocaName -> Text
allocaLLVMEndVar (Ann.AllocaName nm)  = mconcat ["alloca_", nm, "_llvm_end"]

-- | Identifies the machine code base address of an allocation.
allocaMCBaseVar :: Ann.AllocaName -> Text
allocaMCBaseVar (Ann.AllocaName nm) = mconcat ["alloca_", nm, "_mc_base"]

-- | Name of a predicate that checks if a range [start, start+size)]
-- is in the range of an allocation for LLVM addresses.
isInLLVMAlloca :: Ann.AllocaName -> Text
isInLLVMAlloca (Ann.AllocaName nm) = mconcat ["llvmaddr_in_alloca_", nm]

-- | Name of a predicate that checks if a range [start, start+size)]
-- is in the range of an allocation for machine code addresses.
isInMCAlloca :: Ann.AllocaName -> Text
isInMCAlloca (Ann.AllocaName nm) = mconcat ["mcaddr_in_alloca_", nm]

-- | Identifies the LLVM end address of an allocation.
allocaMCEndVar :: Ann.AllocaName -> Text
allocaMCEndVar (Ann.AllocaName nm)  = mconcat ["alloca_", nm, "_mc_end"]

-- | A range @(b,e)@ representing the addresses @[b..e)@.
-- We assume that @b ule e@.
type Range = (SMT.Term, SMT.Term)

-- | @isDisjoint x y@ returns a predicate that holds if the ranges denoted by @x@ and @y@
-- do not overlap.
isDisjoint :: Range -> Range -> SMT.Term
isDisjoint (b0, e0) (b1, e1) = SMT.or [ SMT.bvule e0 b1, SMT.bvule e1 b0 ]

-- | @assumeLLVMDisjoint (base, end) nm@ adds assumptions that @[base,end)@
-- is disjoint from allocation identified by @nm@.
--
-- We can assume @end >= base@ for all allocations
assumeLLVMDisjoint :: Range -> Ann.AllocaName -> BlockVCG ()
assumeLLVMDisjoint r nm = do
  assume $ isDisjoint r (varTerm (allocaLLVMBaseVar nm), varTerm (allocaLLVMEndVar nm))

-- | Check if LLVM type and Macaw types have the same memory layout.
typeCompat :: L.Type -> MemRepr tp -> Bool
typeCompat (L.PrimType (L.Integer lw)) (BVMemRepr mw _) =
  toInteger lw == 8 * intValue mw
typeCompat (L.PtrTo _tp) (BVMemRepr mw _) =
  intValue mw == 64
typeCompat (L.PrimType (L.FloatType L.Float)) (FloatMemRepr SingleFloatRepr _) =
  True
typeCompat (L.PrimType (L.FloatType L.Double)) (FloatMemRepr DoubleFloatRepr _) =
  True
typeCompat (L.PrimType (L.FloatType L.X86_fp80)) (FloatMemRepr X86_80FloatRepr _) =
  True
typeCompat (L.Vector ln ltp) (PackedVecMemRepr mn mtp) =
  toInteger ln == intValue mn && typeCompat ltp mtp
typeCompat _ _ = False

-- | @assertAddrReadOnStack addr cnt msg@ asserts that @[addr..addr+cnt)@
-- is a set of addresses on the stack that have not been allocated to an LLVM
-- allocation.
--
-- @msg@ used as the reason for the check.
assertAddrRangeOnUnallocStack :: SMT.Term -> Integer -> String -> BlockVCG ()
assertAddrRangeOnUnallocStack mcAddr sz msg = do
  proveTrue (evalRangeCheck onMCFunStack mcAddr sz) msg

processMCEvents :: [M.Event] -> BlockVCG [M.Event]
processMCEvents (M.CmdEvent cmd:mevs) = do
  addCommand cmd
  processMCEvents mevs
processMCEvents (M.WarningEvent msg:mevs) = do
  liftIO $ hPutStrLn stderr msg
  processMCEvents mevs
processMCEvents (M.InstructionEvent _curAddr:mevs) = do
  processMCEvents mevs
processMCEvents (M.MCOnlyStackReadEvent mcAddr tp macawValVar:mevs) = do
  -- Assert address is on stack
  do thisIP <- gets mcCurAddr
     assertAddrRangeOnUnallocStack mcAddr (memReprBytes tp) $
       printf "Machine code read at %s is in unreserved stack space." (show thisIP)
  -- Define value from reading Macaw heap
  supType <- getSupportedType tp
  defineVarWithReadValue macawValVar mcAddr supType
  -- Process future events.
  processMCEvents mevs
-- Every LLVM write should have a machine code write (but not
-- necessarily vice versa), we first pattern match on machine code
-- writes.
processMCEvents (M.MCOnlyStackWriteEvent mcAddr tp macawVal:mevs) = do
  -- Update stack with write.
  macawWrite mcAddr tp macawVal
  -- Assert address is on stack
  do addr <- gets mcCurAddr
     let sz = memReprBytes tp
     proveTrue (SMT.term_app onMCFunStack [mcAddr, SMT.bvdecimal sz 64])
               (printf "Machine code write at %s is in unreserved stack space." (show addr))
  -- Process next events
  processMCEvents mevs
-- Fallback case
processMCEvents mevs = pure mevs

-- | Return true if the first address is always less than second.
addrLt :: MemAddr 64 -> MemAddr 64 -> Bool
addrLt x y = addrBase x == addrBase y && addrOffset x < addrOffset y

mcNextAddr :: BlockVCGState -> MemAddr 64
mcNextAddr s = incAddr (toInteger (mcCurSize s)) (segoffAddr (mcCurAddr s))


getNextEvents :: BlockVCG ()
getNextEvents = do
  ctx <- ask
  s <- get
  let addr = mcNextAddr s
  if not (addrLt addr (mcBlockEndAddr ctx)) then
    error $ "Unexpected end of machine code events."
   else do
    let mem = moduleMem (mcModuleVCGContext ctx)
    let Just addrSegOff = asSegmentOff mem addr
    let loc = ExploreLoc { loc_ip = addrSegOff
                         , loc_x87_top = mcX87Top s
                         , loc_df_flag = mcDF s
                         }
    (r, nextIdx, sz) <-
      case M.blockEvents (mcBlockMap ctx) (mcCurRegs s) (mcLocalIndex s) loc of
        Left e -> errorAt e
        Right p -> pure p
    -- Update local index and next addr
    put $! s { mcLocalIndex = nextIdx
             , mcCurAddr  = addrSegOff
             , mcCurSize  = sz
             }
    -- Update events
    modify $ \t -> t { mcEvents = r }

popMCEvent :: BlockVCG M.Event
popMCEvent = do
  evts0 <- gets mcEvents
  evts <- processMCEvents evts0
  ctx <- ask
  nextAddr <- gets mcNextAddr
  case evts of
    [] -> do
      getNextEvents
      popMCEvent
    M.CmdEvent cmd:mevs -> do
      addCommand cmd
      modify $ \s -> s { mcEvents = mevs }
      popMCEvent
    M.WarningEvent msg:mevs -> do
      liftIO $ hPutStrLn stderr msg
      modify $ \s -> s { mcEvents = mevs }
      popMCEvent
    M.InstructionEvent _curAddr:mevs -> do
      modify $ \s -> s { mcEvents = mevs }
      popMCEvent
    M.MCOnlyStackReadEvent mcAddr tp macawValVar:mevs -> do
      -- Assert address is on stack
      do thisIP <- gets mcCurAddr
         assertAddrRangeOnUnallocStack mcAddr (memReprBytes tp) $
           printf "Machine code read at %s is in unreserved stack space." (show thisIP)
      -- Define value from reading Macaw heap
      supType <- getSupportedType tp
      defineVarWithReadValue macawValVar mcAddr supType
      -- Process future events.
      modify $ \s -> s { mcEvents = mevs }
      popMCEvent
    -- Every LLVM write should have a machine code write (but not
    -- necessarily vice versa), we pattern match on machine code
    -- writes.
    M.MCOnlyStackWriteEvent mcAddr tp macawVal:mevs -> do
      -- Update stack with write.
      macawWrite mcAddr tp macawVal
      -- Assert address is on stack
      do addr <- gets mcCurAddr
         let sz = memReprBytes tp
         proveTrue (SMT.term_app onMCFunStack [mcAddr, SMT.bvdecimal sz 64])
           (printf "Machine code write at %s is in unreserved stack space." (show addr))
      -- Process next events
      modify $ \s -> s { mcEvents = mevs }
      popMCEvent
    -- This checks to see if the next instruction jumps to the next ip,
    -- and if so it runs it.
    (M.FetchAndExecuteEvent ectx regs:r)
      -- Check IP in registers matches next register
      | Just ipAddr <- valueAsMemAddr (regs^.boundValue X86_IP)
      , nextAddr == ipAddr
      , addrLt ipAddr (mcBlockEndAddr ctx) -> do
      when (not (null r)) $ do
        error "MC event after fetch and execute"
      modify $ \s -> s { mcEvents = [] }
      -- Update loc_x86_top and loc_df_flag
      case regs^.boundValue X87_TopReg of
        BVValue _w i | 0 <= i, i <= 7 -> do
          modify $ \s -> s { mcX87Top = fromInteger i }
        _ -> error "Unexpected X87_TOP value"
      case regs^.boundValue DF of
        BoolValue b ->
          modify $ \s -> s { mcDF = b }
        _ -> error "Unexpected direction flag"
      -- Update registers
      modify $ \s -> s { mcCurRegs = fmapF (Const . M.primEval ectx) regs }
      -- Process next events
      getNextEvents
      popMCEvent
    (h:r) -> do
      modify $ \s -> s { mcEvents = r }
      pure h

popFetchAndExecute :: BlockVCG ()
popFetchAndExecute = do
  evt <- popMCEvent
  case evt of
    M.FetchAndExecuteEvent ectx regs -> do
      r <- gets mcEvents
      when (not (null r)) $ do
        error "MC event after fetch and execute"
      modify $ \s -> s { mcCurRegs = fmapF (Const . M.primEval ectx) regs }
    _ -> do
      error "Missing fetch and execute event."

-- | Check direction flag is clear.
--
-- This must be checked on calls and returns.
checkDirectionFlagClear :: BlockVCG ()
checkDirectionFlagClear = do
  df <- gets mcDF
  when df $ error "Direction flag must be clear."

-- | Get name to use for SMT purpioses.
getBlockAnn :: BlockLabel -> BlockVCG Ann.BlockAnn
getBlockAnn lbl = do
  fnAnn <- asks $ curFunAnnotations
  case findBlock fnAnn lbl of
    Nothing -> errorAt $ "Could not find annotations for LLVM block " ++ show lbl
    Just b -> pure b

$(pure [])

-- | Return name of variable associated with LLVM identifier.
llvmVar :: Text -> Text
llvmVar nm = "llvm_" <> nm


-- | Return name of variable associated with LLVM identifier.
identVar :: Ident -> Text
identVar (Ident nm) = llvmVar (Text.pack nm)

-- | Return the SMT term equal to the current value at the current
-- program location.
primEval :: Type -> Value' BlockLabel -> BlockVCG SMT.Term
primEval _ (ValIdent var) = do
  pure $! varTerm (identVar var)
primEval (PrimType (Integer w)) (ValInteger i) = do
  when (w <= 0) $ error "primEval given negative width."
  pure $ SMT.bvdecimal i (fromIntegral w)
primEval tp v  = error $ "TODO: Add more support in primEval:\n"
                    ++ "Type:  " ++ show tp ++ "\n"
                    ++ "Value: " ++ show v

$(pure [])

-- | Return the SMT term equal to the current value at the current
-- program location.
evalTyped :: Typed (Value' BlockLabel) -> BlockVCG SMT.Term
evalTyped (Typed tp var) = primEval tp var

$(pure [])

llvmInvoke :: Bool
           -> L.Symbol
           -> [Typed (Value' BlockLabel) ]
           -> (Maybe (Ident, Type))
           -> BlockVCG ()
llvmInvoke isTailCall fsym args lRet = do
  when isTailCall $ error "Tail calls are not yet supported."

  -- Evaluate arguments.
  lArgs <- traverse evalTyped args
  -- Get machine code state at end of the instruction.
  popFetchAndExecute
  regs <- gets mcCurRegs

  let mRegIP = getConst $ regs ^. boundValue X86_IP
  assertFnNameEq fsym mRegIP
  -- Verify that the arguments should be same.  Note: Here we take the
  -- number of arguments from LLVM side, since the number of arguments
  -- in Macaw side seems not explicit.  Also assuming that the # of
  -- arguments of LLVM side is less or equal than six.
  when (length lArgs > length x86ArgRegs) $ do
    error $ "Too many arguments."


  -- Get address of next instruction as an SMT term.
  nextInsnAddr <- gets $  mcNextAddr

  -- Check check pointer is valid and we saved return address on call.
  do let sp = getConst $ regs^.boundValue RSP
     -- Checks that the stack pointer is on the stack.
     assertAddrRangeOnUnallocStack sp 8 "Return address for call is on stack."
     -- Get value stored at return address
     mem <- gets  $ varTerm . memVar . mcMemIndex
     -- Check stored return value matches next instruction
     proveEq (readFromMCMemory mem sp addrSupportedMemType) (M.evalMemAddr nextInsnAddr)
       "Check return address matches next instructiomain."

  do let compareArg :: SMT.Term -> X86Reg (M.BVType 64) -> BlockVCG ()
         compareArg la reg = do
           let Const ma = regs^.boundValue reg
           proveEq la ma "Register matches LLVM"
     zipWithM_ compareArg lArgs x86ArgRegs

  checkDirectionFlagClear

  -- Create registers for instruction after call.
  newRegs <- do
    prover <- asks callbackFns
    let calleeRegValues =
          [ (Some r, getConst $ regs^.boundValue r)
          | r <- calleeSavedGPRegs
          ]
          ++ [(Some RSP, addc (getConst (regs^.boundValue RSP)) 8)]
    liftIO $
      declareAddrStartRegValues prover (addrName nextInsnAddr) calleeRegValues
  modify $ \s -> s { mcCurRegs = newRegs }

  -- Clear all non callee saved registers

  -- If LLVM side has a return value, then we define the LLVM event in
  -- terms of the value bound to RAX for the rest of the program.
  case lRet of
    Just (llvmIdent, tp) -> do
      -- Returned pointers are assumed to be on heap, so we can assume they are equal.
      let mRetVal = getConst $ newRegs^.boundValue RAX
      let smtSort = case tp of
                      PtrTo _ -> SMT.bvSort 64
                      PrimType (Integer 64) -> SMT.bvSort 64
                      _ ->  error $ "TODO: Add support for return type " ++ show tp
      addCommand $ SMT.defineFun (identVar llvmIdent) [] smtSort  mRetVal
    Nothing -> pure ()

-- | Add the LLVM declarations for an allocation.
allocaDeclarations :: Ann.AllocaName
                   -> Natural -- Size
                   -> BlockVCG ()
allocaDeclarations nm isz = do
  let sz = SMT.bvdecimal (toInteger isz) 64
  -- Declare LLVM alloca base
  addCommand $ SMT.declareConst (allocaLLVMBaseVar nm) ptrSort
  let llvmBase = varTerm (allocaLLVMBaseVar nm)
  -- Assert alloca base is not too large.
  assume $ SMT.bvult llvmBase (SMT.bvneg sz)
  -- Define LLVM alloca end
  addCommand $ SMT.defineFun (allocaLLVMEndVar nm) [] ptrSort $ SMT.bvadd llvmBase [sz]
  let llvmEnd = varTerm (allocaLLVMEndVar nm)
  -- Introduce predicate to check LLVM addresses.
  addCommand $ defineRangeCheck (isInLLVMAlloca nm) llvmBase llvmEnd
  -- Add assumption that LLVM allocation does not overlap with existing allocations.
  do used <- gets mcUsedAllocas
     when (Set.member nm used) $ error $ show nm ++ " is already used an allocation."
     mapM_ (assumeLLVMDisjoint (llvmBase,llvmEnd)) used
     modify $ \s -> s { mcUsedAllocas = Set.insert nm (mcUsedAllocas s) }

  -- Define register alloca is returned to.
  addCommand $ SMT.defineFun (llvmVar (Ann.allocaNameText nm)) [] ptrSort (varTerm (allocaLLVMBaseVar nm))


-- | This updates the state for an LLVM allocation
llvmAlloca :: Ident -- ^ Identifier to assign this to.
           -> Type -- ^ Type of elements
           -> Maybe (Typed (Value' BlockLabel)) -- ^ Number of elements (Nothing = 1)
           -> Maybe Int -- ^ Required alignment
           -> BlockVCG ()
llvmAlloca (Ident nm0) ty eltCount _malign = do
  let eltSize :: Integer
      eltSize =
        case ty of
          PrimType (Integer i) | i .&. 0x7 == 0 -> toInteger i `shiftR` 3
          PtrTo _ -> 8
          _ -> error $ "Unexpected type " ++ show ty
  let nm = Ann.AllocaName (Text.pack nm0)

  allocaMap <- gets $ mcPendingAllocaOffsetMap
  a <-
    case Map.lookup nm allocaMap of
      Nothing ->
        error $ "Could not find offset of alloca with name: " ++ show nm ++ "\n"
          ++ "Names: " ++ show (Map.keys allocaMap)
      Just a -> pure a

  -- Delete this from pending allocations
  modify $ \s -> s { mcPendingAllocaOffsetMap = Map.delete nm allocaMap }

  -- Get total size as a bv64
  llvmSize <-
    case eltCount of
      Nothing -> pure $ SMT.bvdecimal eltSize 64
      Just (Typed itp@(PrimType (Integer 64)) i) -> do
        cnt <- primEval itp i
        pure $ SMT.bvmul (SMT.bvdecimal eltSize 64) [cnt]
      Just (Typed itp _) -> do
        error $ "Unexpected count type " ++ show itp
  proveEq (SMT.bvdecimal (toInteger (Ann.allocaSize a)) 64) llvmSize
    "Size provided to LLVM matches expected size."
  -- Create declarations for alloca.
  allocaDeclarations nm (Ann.allocaSize a)

llvmLoad :: SMT.Term
         -> L.Type
         -> Var
         -> BlockVCG ()
llvmLoad llvmAddr llvmType llvmValVar = do
  mevt <- popMCEvent
  case mevt of
    M.JointStackReadEvent mcAddr mcType macawValVar allocName -> do
      -- Check size of writes are equivalent.
      unless (typeCompat llvmType mcType) $ do
        error "Incompatible LLVM and machine code types."
      let sz = memReprBytes mcType
      -- Check alloca is defined
      do used <- gets $ mcUsedAllocas
         when (not (Set.member allocName used)) $ error $ "Unknown allocation: " ++ show allocName
      -- Prove: LLVM address is in allocation
      proveTrue (evalRangeCheck (isInLLVMAlloca allocName) llvmAddr sz)
                (printf "Check LLVM write address targets %s allocation." (show allocName))
      -- Prove: machine code addres is in allocation.
      proveTrue (evalRangeCheck (isInMCAlloca allocName) mcAddr sz)
                (printf "Check machine code write address targets %s allocation." (show allocName))
      -- Assert machine code address is same offset of machine code region as LLVM address.
      let llvmOffset = SMT.bvsub llvmAddr (varTerm (allocaLLVMBaseVar allocName))
      let mcOffset   = SMT.bvsub   mcAddr (varTerm (allocaMCBaseVar allocName))
      proveEq llvmOffset mcOffset
        "LLVM and machine code read from same allocation offset."
      -- Define value from reading Macaw heap
      supType <- getSupportedType mcType
      defineVarWithReadValue macawValVar mcAddr supType
      let smtType = supportedSort supType
      -- Define LLVM value
      addCommand $ SMT.defineFun llvmValVar [] smtType (varTerm macawValVar)
    M.HeapReadEvent mcAddr mcType macawValVar -> do
      -- Assert addresses are the same
      proveEq mcAddr llvmAddr
        ("Machine code heap load address matches expected from LLVM")
      -- Add that macaw points to the heap
      do addr <- gets mcCurAddr
         proveTrue (inHeapSegment mcAddr (memReprBytes mcType))
           (printf "Read from heap at %s is valid." (show addr))

      -- Check size of writes are equivalent.
      unless (typeCompat llvmType mcType) $ do
        error "Incompatible LLVM and machine code types."
      -- Define value from reading Macaw heap
      supType <- getSupportedType mcType
      defineVarWithReadValue macawValVar mcAddr supType
      let smtType = supportedSort supType
      -- Define LLVM value returned in terms of macaw value
      addCommand $ SMT.defineFun llvmValVar [] smtType (varTerm macawValVar)
    _ -> do
      error "Expected a machine code load event."

-- | Handle an LLVM store.
llvmStore :: SMT.Term -- ^ Address
          -> L.Type -- ^ LLVM type
          -> SMT.Term -- ^ Value written
          -> BlockVCG ()
llvmStore llvmAddr llvmType llvmVal = do
  mevt <- popMCEvent
  case mevt of
    M.JointStackWriteEvent mcAddr mcType mcVal allocName -> do
      -- Check the number of bytes written are the same.
      unless (typeCompat llvmType mcType) $ do
        errorAt $ "Machine code and LLVM writes have incompatible types:\n"
            ++ "MC type:   " ++ show mcType ++ "\n"
            ++ "LLVM type: " ++ show llvmType

      let llvmAllocaBase :: SMT.Term
          llvmAllocaBase = varTerm ("llvm_" <> Ann.allocaNameText allocName)
      let mcAllocaBase :: SMT.Term
          mcAllocaBase = varTerm (allocaMCBaseVar allocName)
      -- Steps:
      let sz = memReprBytes mcType
      -- Prove: machine code addres is valid.
      proveTrue (evalRangeCheck (isInMCAlloca allocName) mcAddr sz)
                (printf "Check machine code write is in range is in %s alloca." (show allocName))
      -- Prove: llvmAddr - llvmAllocaBase = mcAddr - mcAllocaBase
      let llvmOffset = SMT.bvsub llvmAddr llvmAllocaBase
      let mcOffset   = SMT.bvsub   mcAddr   mcAllocaBase
      proveEq llvmOffset mcOffset "LLVM and machine code write to same allocation offset."
      -- Assert values are equal
      thisIP <- gets mcCurAddr
      proveEq llvmVal mcVal $
        (printf "Value written at addr %s equals LLVM value." (show thisIP))
    M.HeapWriteEvent _mcAddr mcType mcVal -> do
      -- Check types agree.
      unless (typeCompat llvmType mcType) $ do
        error "Macaw and LLVM writes have different types."
      missingFeature "Assert machine code and llvm heap write addresses are equal."
      -- Assert values are equal
      proveEq llvmVal mcVal
        ("Machine code heap store matches expected from LLVM")
    _ -> do
      error "llvmStore: Expected a Macaw heap or joint stack write event."

llvmReturn :: Maybe (Typed (Value' BlockLabel)) -> BlockVCG ()
llvmReturn mlret = do
  -- Get register values after return.
  popFetchAndExecute
  regs <- gets mcCurRegs
  -- Assert the IP after the fetch and execute is the return address
  proveEq (getConst (regs^.boundValue X86_IP)) (varTerm "return_addr")
    "Return address matches entry value."

  -- Assert the stack height at the return is just above the return
  -- address pointer.
  proveEq (getConst (regs^.boundValue RSP)) (addc (varTerm "stack_high") 8)
    "Stack height at return matches init."

  checkDirectionFlagClear

  do forM_ calleeSavedGPRegs $ \r -> do
       proveEq (getConst (regs^.boundValue r)) (varTerm (functionStartRegValue r))
          (printf "Value of %s at return is preserved." (show r))


  missingFeature "Processor is in x87 (as opposed to MMX mode)"
  missingFeature "MXCSR control bits must be preserved."
  missingFeature "X87 control word must be preserved."

  -- Assert the value in RAX is the return value.
  case mlret of
    Nothing -> pure ()
    Just (Typed llvmTy v) -> do
      case llvmTy of
        PtrTo _ -> do
          lret <- primEval llvmTy v
          let mret = getConst $ regs^.boundValue RAX
          proveEq lret mret "Return values match"
        PrimType (Integer 64) -> do
          lret <- primEval llvmTy v
          let mret = getConst $ regs^.boundValue RAX
          proveEq lret mret "Return values match"
        PrimType (Integer n) | 0 < n, n < 64 -> do
          lret <- primEval llvmTy v
          let mret = SMT.extract (fromIntegral (n-1)) 0 (getConst $ regs^.boundValue RAX)
          proveEq lret mret "Return values match"
        _ -> do
          error "Unexpected return value"

$(pure [])

defineTerm :: Ident -> SMT.Sort -> SMT.Term -> BlockVCG ()
defineTerm nm tp t = do
  addCommand $ SMT.defineFun (identVar nm) [] tp t


$(pure [])

llvmError :: String -> a
llvmError msg = error ("[LLVM Error] " ++ msg)

arithOpFunc :: ArithOp
            -> SMT.Term
            -> SMT.Term
            -> SMT.Term
arithOpFunc (Add _uw _sw) x y = SMT.bvadd x [y]
arithOpFunc (Sub _uw _sw) x y = SMT.bvsub x y
arithOpFunc (Mul _uw _sw) x y = SMT.bvmul x [y]
arithOpFunc _ _ _ = llvmError "Not implemented yet"

-- | Convert LLVM type to SMT sort.
asSMTSort :: Type -> Maybe SMT.Sort
asSMTSort (PtrTo _) = Just (SMT.bvSort 64)
asSMTSort (PrimType (Integer i)) | i > 0 = Just $ SMT.bvSort (fromIntegral i)
asSMTSort _ = Nothing

-- | Return the definition in the module with the given name.
getDefineByName :: Module -> String -> Maybe Define
getDefineByName llvmMod name =
  List.find (\d -> defName d == Symbol name) (modDefines llvmMod)

$(pure [])

-- | Register values initialized from annotations.
initBlockRegValues :: Ann.BlockAnn -> [(Some X86Reg, SMT.Term)]
initBlockRegValues blockAnn =
  [ (Some X86_IP,     SMT.bvhexadecimal (toInteger (Ann.blockAddr blockAnn)) 64)
  , (Some X87_TopReg, SMT.bvdecimal (toInteger (Ann.blockX87Top blockAnn)) 3)
  , (Some DF,         if Ann.blockDFFlag blockAnn then SMT.true else SMT.false)
  ]

$(pure [])

evalAnnPred :: RegState X86Reg (Const SMT.Term)
            -> SMT.Term -- ^ Current state of memory
            -> Ann.Expr Ann.BlockVar
            -> SMT.Term
evalAnnPred regs mem e = do
  let r = evalAnnPred regs mem
  case e of
    Ann.Eq x y -> SMT.eq [r x, r y]
    Ann.BVSub x y -> SMT.bvsub (r x) (r y)
    Ann.BVDecimal x y -> SMT.bvdecimal (toInteger x) y
    Ann.Var v ->
      case v of
        Ann.StackHigh -> varTerm "stack_high"
        Ann.InitGPReg64 reg -> getConst $ regs^.boundValue (X86_GP reg)
        Ann.FnStartGPReg64 reg ->
          varTerm (functionStartRegValue (X86_GP reg))
        Ann.MCStack addr bitCount ->
          readFromMCMemory mem (r addr) (supportedBVMemType (bitCount `shiftR` 3))

$(pure [])

-- | @verifyBlockPreconditions f lbl@ verifies the preconditions for block @f@
-- are satisfied in the current state.
--
-- The function @f@ is applied to each predicate before verification,
-- and allows us to conditionally validate some of the preconditions.
verifyBlockPreconditions :: String
                         -> (SMT.Term -> SMT.Term)
                         -> BlockLabel
                         -> BlockVCG ()
verifyBlockPreconditions prefix f lbl = do
  tgtBlockAnn <- getBlockAnn lbl
  firstLabel <- asks firstBlockLabel

  when (lbl == firstLabel) $ fail "Do not support jumping to first label in function."

  regs <- gets mcCurRegs
  mem  <- gets $ varTerm . memVar . mcMemIndex
  -- Check initialized register values
  forM_ (initBlockRegValues tgtBlockAnn) $ \(Some r, expected) -> do
    -- Get register values
    let mcValue = getConst $ regs ^. boundValue r
    proveTrue (f (SMT.eq [expected, mcValue])) $ printf "Checking %s register %s." prefix (show r)

  -- Check preconditions
  forM_ (Ann.blockPreconditions tgtBlockAnn) $ \p -> do
    proveTrue (f (evalAnnPred regs mem p)) $ printf "Checking %s precondition." prefix

-- | Process LLVM and macaw events to ensure they are equivalent.
stmtsEq :: [L.Stmt]
        -> BlockVCG ()
stmtsEq (stmt@(L.Result ident inst _mds):stmts) = do
  setCurrentLLVMStmt stmt
  case inst of
    Arith lop (Typed lty lhs) rhs
      | Just tp <- asSMTSort lty -> do
          lhsv   <- primEval lty lhs
          rhsv   <- primEval lty rhs
          defineTerm ident tp $ arithOpFunc lop lhsv rhsv
          stmtsEq stmts
    ICmp lop (Typed lty@(PrimType (Integer w)) lhs) rhs -> do
      when (w <= 0) $ error $ "Unexpected bitwidth " ++ show w
      lhsv <- primEval lty lhs
      rhsv <- primEval lty rhs
      let r =
            case lop of
              Ieq -> SMT.eq [lhsv, rhsv]
              Ine -> SMT.distinct [lhsv, rhsv]
              Iugt -> SMT.bvugt lhsv rhsv
              Iuge -> SMT.bvuge lhsv rhsv
              Iult -> SMT.bvult lhsv rhsv
              Iule -> SMT.bvule lhsv rhsv
              Isgt -> SMT.bvsgt lhsv rhsv
              Isge -> SMT.bvsge lhsv rhsv
              Islt -> SMT.bvslt lhsv rhsv
              Isle -> SMT.bvsle lhsv rhsv
      defineTerm ident (SMT.bvSort 1) (SMT.ite r (SMT.bvdecimal 1 1) (SMT.bvdecimal 0 1))
      stmtsEq stmts
    Alloca ty eltCount malign -> do
      llvmAlloca ident ty eltCount malign
      stmtsEq stmts
    Load (Typed (PtrTo lty) src) _ord _align -> do
      addrTerm <- primEval (PtrTo lty) src
      llvmLoad addrTerm lty (identVar ident)
      stmtsEq stmts
    Call isTailCall retty f args -> do
      -- Evaluate function
      fSym <- case f of
                ValSymbol s -> pure s
                _ -> fail $ "VCG currently only supports direct calls."
      -- Add invoke event
      llvmInvoke isTailCall fSym args (Just (ident, retty))
      stmtsEq stmts
    _ -> do
      error $ "stmtsEq: unsupported instruction: " ++ show inst
stmtsEq (stmt@(L.Effect instr _mds):stmts) = do
  setCurrentLLVMStmt stmt
  case instr of
    Store llvmVal llvmPtr _ordering _align -> do
      addrTerm <- evalTyped llvmPtr
      valTerm  <- evalTyped llvmVal
      llvmStore addrTerm (typedType llvmVal) valTerm
      stmtsEq stmts
    Br (Typed _ty cnd) tlbl flbl -> do
      cndTerm <- primEval (PrimType (Integer 1)) cnd
      let c = SMT.eq [cndTerm, SMT.bvdecimal 1 1]

      unless (null stmts) $ error "Expected branch as last LLVM statement."
      popFetchAndExecute

      -- Verify block preconditions.
      verifyBlockPreconditions "true branch"  (SMT.implies [c])         tlbl
      verifyBlockPreconditions "false branch" (SMT.implies [SMT.not c]) flbl


    Jump lbl -> do
      unless (null stmts) $ error "Expected jump to be last LLVM statement."
      popFetchAndExecute

      -- Verify block preconditions
      verifyBlockPreconditions "jump" id lbl

    Ret retVal -> do
      unless (null stmts) $ error "Expected return to be last LLVM statement."
      llvmReturn (Just retVal)
    RetVoid ->
      unless (null stmts) $ error "Expected return to be last LLVM statement."
      llvmReturn Nothing
    _ -> error "Unsupported instruction."
stmtsEq [] = do
  error $ "We have reached end of LLVM events without a block terminator."

$(pure [])

-- | Emit an SMT command to the solver.
writeCommand :: Handle -> SMT.Command -> IO ()
writeCommand h (SMT.Cmd b) =
  LText.hPutStrLn h (Builder.toLazyText b)

$(pure [])

-- | Information needed for interatively verifying goal.
data InteractiveContext = InteractiveContext
  { ictxAnnFile :: !FilePath
     -- ^ Name of YAML file for error-reporting purposes
  , ictxFunName :: !String
    -- ^ Name of function to verify
  , ictxBlockLabel :: !String
     -- ^ Label of block
  , ictxAllGoalCounter :: !(IORef Natural)
    -- ^ Counter for all goals
  , ictxVerifiedGoalCounter :: !(IORef Natural)
    -- ^ Counter for all successfully verified goals.
  , ictxBlockGoalCounter :: !(IORef Natural)
    -- ^ Index of goal to discharge within block
  , ictxCmdHandle :: !Handle
     -- ^ Handle for sending commands to
  , ictxRespHandle :: !Handle
     -- ^ Handle for reading responses from
  , ictxErrHandle :: !Handle
     -- ^ Handle for reading errors from.
  }

-- | This reads input from the error handle while waiting for the async thread to terminate.
waitForResponse :: Handle -> ASync.Async a -> IO a
waitForResponse errHandle action = do
  pollHandle <- ASync.async $ hGetLine errHandle
  r <- ASync.waitEither pollHandle action
  case r of
    Left e -> do
      hPutStrLn stderr $ "Solver message: " ++ show e
      waitForResponse errHandle action
    Right resp -> do
      ASync.cancel pollHandle
      pure resp

-- | Function to verify a SMT proposition is unsat.
interactiveVerifyGoal :: InteractiveContext -- ^ Context for verifying goals
                      -> SMT.Term
                         -- ^ Negation of goal to verify
                      -> String
                         -- ^ Name of proposition for reporting purposes.
                      -> IO ()
interactiveVerifyGoal ictx ng propName = do
  let annFile = ictxAnnFile ictx
  let funName = ictxFunName ictx
  let lbl = ictxBlockLabel ictx
  let cmdHandle = ictxCmdHandle ictx
  let respHandle = ictxRespHandle ictx

  cnt <- readIORef (ictxBlockGoalCounter ictx)
  modifyIORef' (ictxAllGoalCounter ictx)      (+1)
  modifyIORef' (ictxBlockGoalCounter ictx)    (+1)
  let fname = standaloneGoalFilename funName lbl cnt
  hPutStrLn stderr $ printf "Verifying %s: %s" fname propName
  writeCommand cmdHandle $ SMT.checkSatAssuming [ng]
  hFlush cmdHandle
  asyncResp <-ASync.async (SMTP.readCheckSatResponse respHandle)
  resp <- waitForResponse (ictxErrHandle ictx) asyncResp
  case resp of
    SMTP.SatResponse -> do
      hPutStrLn stderr $ "Verification failed"
      hPutStrLn stderr ""
      hPutStrLn stderr $ printf "To see output, run `reopt-vcg %s --export <dir>`." annFile
      hPutStrLn stderr $ "The result will be stored in " ++ fname
    SMTP.UnsatResponse -> do
      modifyIORef' (ictxVerifiedGoalCounter ictx) (+1)
      hPutStrLn stderr $ "  Verified"
    SMTP.UnknownResponse -> do
      hPutStrLn stderr $ "Unknown verification result"
      hPutStrLn stderr ""
      hPutStrLn stderr $ printf "To see output, run `reopt-vcg %s --export <dir>`." annFile
      hPutStrLn stderr $ "The result will be stored in " ++ fname
    SMTP.CheckSatUnsupported -> do
      hPutStrLn stderr $ "Verification failed"
      hPutStrLn stderr $ "The SMT solver does not support check-sat-assuming."
    (SMTP.CheckSatError msg) -> do
      hPutStrLn stderr $ "Verification failed"
      hPutStrLn stderr $ "The SMT solver returned the following message after check-sat-assuming:"
      hPutStrLn stderr ""
      hPutStrLn stderr $ "  " ++ msg

newInteractiveSession :: FilePath -- ^ Path for annotations
                      -> String -- ^ Command line for SMT solver
                      -> IORef Natural -- ^ Counter for each goal
                      -> IORef Natural -- ^ Counter for each verified goal.
                      -> FunctionName -- ^ Name of function
                      -> String -- ^ Block label for this session.
                      -> (ProverInterface -> IO a)
                      -> IO a
newInteractiveSession annFile cmdline allGoalCounter verifiedGoalCounter funName lbl action = do
  -- Create Goal counter for just this block.
  blockGoalCounter <- newIORef 0
  let cp = (shell cmdline)
           { std_in = CreatePipe
           , std_out = CreatePipe
           , std_err = CreatePipe
           }
  createResult <- try $ createProcess cp
  case createResult of
    Right (Just cmdHandle, Just respHandle, Just errHandle, ph) -> do
      flip finally (terminateProcess ph) $ do
        writeCommand cmdHandle $ SMT.setLogic SMT.allSupported
        writeCommand cmdHandle $ SMT.setProduceModels True
        let ictx = InteractiveContext { ictxAnnFile = annFile
                                      , ictxFunName = funName
                                      , ictxBlockLabel = lbl
                                      , ictxAllGoalCounter = allGoalCounter
                                      , ictxVerifiedGoalCounter = verifiedGoalCounter
                                      , ictxBlockGoalCounter = blockGoalCounter
                                      , ictxCmdHandle = cmdHandle
                                      , ictxRespHandle = respHandle
                                      , ictxErrHandle = errHandle
                                      }
        let fns = ProverInterface
                    { addCommandCallback = \cmd -> do
                        writeCommand cmdHandle cmd
                    , proveFalseCallback = \p msg -> do
                        interactiveVerifyGoal ictx p msg
                    , proveTrueCallback = \p msg -> do
                        interactiveVerifyGoal ictx (SMT.not p) msg
                    }
        r <- action fns
        writeCommand cmdHandle $ SMT.exit
        pure r
    Right _ -> do
      hPutStrLn stderr $ "Unexpected failure running " ++ cmdline
      exitFailure
    Left err -> do
      hPutStrLn stderr $ "Could not execute " ++ cmdline
      hPutStrLn stderr $ "  " ++ show (err :: IOException)
      exitFailure

-- | This runs an action with a proof session generator, and reports
-- the final proof results.
interactiveSMTGenerator :: FilePath -- ^ Name of yaml file for error reporting purposes.
                        -> String -- ^ Command line for running SMT solver
                        -> IO ProverSessionGenerator
interactiveSMTGenerator annFile cmdline = do
  -- Counter for all goals
  allGoalCounter <- newIORef 0
  -- Counter for goals successfully verified.
  verifiedGoalCounter <- newIORef 0
  let whenDone = do
        allCnt <- readIORef allGoalCounter
        verCnt <- readIORef verifiedGoalCounter
        if verCnt < allCnt then do
          hPutStrLn stdout "Verification Failed"
         else do
          hPutStrLn stdout "All verifications succeeded"
        hPutStrLn stdout $ "Verified " ++ show verCnt ++ "/" ++ show allCnt ++ " Goals."
  pure $! PSGen { blockCallbacks =
                    newInteractiveSession annFile cmdline allGoalCounter verifiedGoalCounter
                , sessionComplete = whenDone
                }

exportCheckSatProblem :: FilePath
                         -- ^ Directory to write file to.
                      -> String -- ^ Name of function
                      -> String -- ^ Name of label of block we are generating.
                      -> IORef Natural -- ^ Index of goal to discharge within block
                      -> IORef Builder.Builder  -- ^ A representation of all commands added.
                      -> SMT.Term -- ^ Proposition to assert and check unsat of.
                      -> String   -- ^ Name of goal to prove
                      -> IO ()
exportCheckSatProblem outDir fn lbl goalCounter cmdRef negGoal msg = do
  cnt <- readIORef goalCounter
  modifyIORef' goalCounter (+1)
  cmds <- readIORef cmdRef
  let fname = standaloneGoalFilename fn lbl cnt
  hPutStrLn stdout $ fname ++ ": " ++ msg
  bracket (openFile (outDir </> fname) WriteMode) hClose $ \h -> do
    writeCommand h $ comment (fromString msg)
    writeCommand h $ SMT.setLogic SMT.allSupported
    writeCommand h $ SMT.setProduceModels True
    -- Write commands from proof state
    LText.hPutStr h (Builder.toLazyText cmds)
    writeCommand h $ SMT.checkSatAssuming [negGoal]
    writeCommand h $ SMT.exit

exportCallbacks :: FilePath -- ^ Directory to write file to.
                -> FunctionName -- ^ Name of function
                -> String -- ^ Block label
                -> (ProverInterface -> IO a)
                -> IO a
exportCallbacks outDir fn lbl action = do
  goalCounter <- newIORef 0
  cmdRef <- newIORef mempty
  action $! ProverInterface
    { addCommandCallback = \(SMT.Cmd cmd) -> do
        modifyIORef' cmdRef $ \s -> s <> cmd <> "\n"
    , proveFalseCallback = \p msg ->
        exportCheckSatProblem outDir fn lbl goalCounter cmdRef p msg
    , proveTrueCallback = \p msg ->
        exportCheckSatProblem outDir fn lbl goalCounter cmdRef (SMT.not p) msg
    }

runVCGs :: Ann.FunctionAnn -- ^ Annotations for the function we are verifying.
        -> BlockLabel -- ^ Label of first block
        -> BlockLabel -- ^ Label of this block.
        -> Ann.BlockAnn -- ^ Annotations for the block we are verifying.
        -> BlockVCG ()
        -> ModuleVCG ()
runVCGs funAnn firstLabel lbl blockAnn action = do
  let isFirstBlock = firstLabel == lbl
  modCtx <- ask
  let mem = moduleMem modCtx
  thisSegOff <- do
    let absAddr = fromIntegral (Ann.blockAddr blockAnn)
    case resolveAbsoluteAddr mem absAddr of
      Just o -> pure o
      Nothing -> moduleThrow $ "Could not resolve " ++ show absAddr

  gen <- asks proverGen
  liftIO $ blockCallbacks gen (Ann.llvmFunName funAnn) (Ann.blockLabel blockAnn) $ \prover -> do
    let blockStart = Ann.blockAddr blockAnn
    let sz = Ann.blockCodeSize blockAnn
    let blockEnd =
          if toInteger blockStart + sz < toInteger (maxBound :: Word64) then
            blockStart + fromInteger sz
          else
            error $ "Block overflows memory."
    let blockMap = Map.fromList
          [ (segOff, Ann.eventInfo e)
          | e <- Ann.blockEvents blockAnn
            -- Get segment offset of event.
          , let ea = Ann.eventAddr e
          , let segOff =
                  if blockStart <= ea && ea < blockEnd then
                    case incSegmentOff thisSegOff (toInteger (ea - blockStart)) of
                      Just so -> so
                      Nothing -> error "Block extends outside of starting memory segment"
                   else
                    error $ printf "Memory event annotation address %0x is out of range (low: %0x, high: %0x)." ea blockStart blockEnd
          ]

    let ctx = BlockVCGContext { mcModuleVCGContext = modCtx
                              , curFunAnnotations = funAnn
                              , firstBlockLabel = firstLabel
                              , callbackFns = prover
                              , mcBlockEndAddr = incAddr sz (segoffAddr thisSegOff)
                              , mcBlockMap = blockMap
                              }
    -- Add builtin functions
    do addCommandCallback prover $ M.evenParityDecl
       -- Add read/write operations (independent of registers)
       mapM_ (addCommandCallback prover . supportedReadDecl)  supportedMemTypes
       mapM_ (addCommandCallback prover . supportedWriteDecl) supportedMemTypes
    -- Declare stack and heap bounds.
    mapM_ (addCommandCallback prover) $ mcMemDecls (Ann.stackSize funAnn) (Ann.blockAllocas blockAnn)

    -- Declare registers when function starts.
    declareFunctionStartRegValues prover
    -- Create registers
    regs <- do
      -- Register values determined by location.
      let locRegValues = initBlockRegValues blockAnn
      -- Implicit RSP constraint on block.
      let rspRegValues
             | isFirstBlock = [(Some RSP, varTerm "stack_high")]
             | otherwise = []
      let calleeRegValues
            | isFirstBlock =
              [ (Some r, varTerm (functionStartRegValue r))
              | r <- calleeSavedGPRegs
              ]
            | otherwise =
              []
      declareAddrStartRegValues prover (addrName (segoffAddr thisSegOff))
        (locRegValues ++ rspRegValues ++ calleeRegValues)
    -- Create block state.
    let s = BlockVCGState { mcCurAddr   = thisSegOff
                          , mcCurSize   = 0
                          , mcX87Top    = Ann.blockX87Top blockAnn
                          , mcDF        = Ann.blockDFFlag blockAnn
                          , mcCurRegs  = regs
                          , mcMemIndex = 0
                          , mcUsedAllocas = Set.empty
                          , mcEvents = []
                          , mcLocalIndex = 0
                          , mcPendingAllocaOffsetMap =
                              Map.fromList
                              [ (Ann.allocaName a, a)
                              | a <- Ann.blockAllocas blockAnn
                              , not (Ann.allocaExisting a)
                              ]
                          }
    seq ctx $ seq s $ unBlockVCG action ctx (\() _ -> pure ()) s

data VerificationMode
   = DefaultMode
   | ExportMode !FilePath
   | RunSolver !String

isDefault :: VerificationMode -> Bool
isDefault DefaultMode = True
isDefault _ = False

data VCGArgs
   = VCGArgs { reoptYaml :: !(Maybe FilePath)
               -- ^ Location with yaml file.
             , requestedMode :: !VerificationMode
             }

data VCGCommand
  = ShowHelp
  | RunVCG !VCGArgs

parseArgs :: [String] -> VCGArgs -> Except String VCGCommand
parseArgs cmdArgs args = seq args $
  case cmdArgs of
    [] -> pure $! RunVCG args
    ("--help":_) -> pure $! ShowHelp
    ("--export":path:rest) -> do
      unless (isDefault (requestedMode args)) $ do
        throwError $ "Cannot specify --export or --solver multiple times."
      parseArgs rest $ args { requestedMode = ExportMode path }
    ("--solver":cmdline:rest) -> do
      unless (isDefault (requestedMode args)) $ do
        throwError $ "Cannot specify --export or --solver multiple times."
      parseArgs rest $ args { requestedMode = RunSolver cmdline }
    (path:rest) -> do
      when ("--" `isPrefixOf` path) $ do
        throwError $ "Unexpected flag " ++ path
      when (isJust (reoptYaml args)) $ do
        throwError $ "Multiple VCG files specified."
      parseArgs rest $ args { reoptYaml = Just path }

showHelp :: IO ()
showHelp = do
  putStrLn
    $  "reopt-vcg generates verification conditions to prove that reopt generated\n"
    ++ "   LLVM is faithful to the input binary.\n"
    ++ "Usage: reopt-vcg <input.yaml> {--export <export-dir> | --solver <solver-path>}"

showError :: String -> IO a
showError msg = do
  hPutStrLn stderr $ "Error: " ++ msg
  hPutStrLn stderr $ "Run `reopt-vcg --help` for additional information."
  exitFailure

withVCGArgs :: IO (Ann.MetaVCGConfig, ProverSessionGenerator)
withVCGArgs = do
  cmdArgs <- getArgs
  let initVCG = VCGArgs { reoptYaml = Nothing, requestedMode = DefaultMode }
  args <-
    case runExcept (parseArgs cmdArgs initVCG) of
      Left msg ->
        showError msg
      Right ShowHelp -> do
        showHelp
        exitSuccess
      Right (RunVCG a) -> pure a

  -- Get path to YAML
  annFile <-
    case reoptYaml args of
      Nothing -> showError "Missing VCG file to run."
      Just path -> return path
  cfg <- do
    vcgResult <- Yaml.decodeFileWithWarnings annFile
    case vcgResult of
      Left err -> do
        hPutStrLn stderr $ "Error parsing Yaml: " ++ show err
        exitFailure
      Right (warnings, cfg) -> do
        when (not (null warnings)) $ do
          hPutStrLn stderr $ "Warnings when parsing Yaml file:"
          forM_ warnings $ \warn -> do
            hPutStrLn stderr $ "  " ++ show warn
          exitFailure
        pure cfg
  case requestedMode args of
    ExportMode outdir -> do
      r <- try $ createDirectoryIfMissing True outdir
      case r of
        Right () -> do
          putStrLn $ "Writing output to " ++ outdir
          let psGen = PSGen { blockCallbacks =  exportCallbacks outdir
                            , sessionComplete = pure ()
                            }
          pure (cfg, psGen)
        Left e -> do
          hPutStrLn stderr $ "Error creating output directory: " ++ outdir
          hPutStrLn stderr $ "  " ++ show (e :: IOError)
          exitFailure
    DefaultMode -> do
      psGen <- interactiveSMTGenerator annFile "cvc4 --lang=smt2 --incremental"
      pure (cfg, psGen)
    RunSolver cmdline -> do
      psGen <- interactiveSMTGenerator annFile cmdline
      pure (cfg, psGen)

standaloneGoalFilename :: String -- ^ Name of function to verify
                       -> String  -- ^ Pretty printed version of block label.
                       -> Natural -- ^ Index of goal to discharge within block
                       -> FilePath
standaloneGoalFilename fn lbl i = fn ++ "_" ++ lbl ++ "_" ++ show i ++ ".smt2"


-- | Define LLVM arguments in terms of machine code registers.
--
-- Note. Our current assumption is that LLVM arguments may only be
-- directly referenced in the initial block, and the arguments are
-- only defined in the initial block.  If this changes, then the code
-- below is incorrect as it assumes the value of the registers at the
-- block start is the same as at function start.
defineLLVMArgs :: AddrName
                  -- ^ Pretty printed name of function start address
                  -- for display purposes
               -> [Typed Ident]
               -> [X86Reg (M.BVType 64)] -- ^ Remaining registers for arguments.
               -> BlockVCG ()
defineLLVMArgs _ [] _x86Regs = pure ()
defineLLVMArgs nm (Typed (PrimType (Integer 64)) val : rest) x86Regs =
  case x86Regs of
    [] -> error $ "Ran out of register arguments."
    (reg:restRegs) -> do
      addCommand $ SMT.defineFun (identVar val) [] (SMT.bvSort 64)
                                 (varTerm (addrStartRegValue nm reg))
      defineLLVMArgs nm rest restRegs
defineLLVMArgs _ (Typed tp _val : _rest) _x86Regs =
  error $ "Unexpected type " ++ show tp



-- | Return true if the allocations overlap in memory.
allocaOverlap :: Ann.AllocaInfo -> Ann.AllocaInfo -> Bool
allocaOverlap x y = (ylow - Ann.allocaSize y < xlow) && (xlow - Ann.allocaSize x < ylow)
  where xlow  = Ann.allocaBinaryOffset x
        ylow  = Ann.allocaBinaryOffset y

-- | Return a list containing all distinct overlapping pairs of
-- allocations.
--
-- Note. This is a fairly naive algorithm that is quadratic with
-- respect to the number of allocations @n@.  We could detect whether
-- any overlaps exist by using a @n*log n@ algorithm, but in practice
-- the number of allocations is expected to be quite small, so we have
-- not done so.
allocaOverlaps :: [Ann.AllocaInfo] -- ^ Existing overlaps to check against.
               -> [Ann.AllocaInfo] -- ^ Unprocessed overlaps.
               -> [(Ann.AllocaInfo, Ann.AllocaInfo)]
allocaOverlaps _prev [] = []
allocaOverlaps prev (h:r) =
  [ (x,h) | x <- reverse (filter (`allocaOverlap` h) prev) ]
  ++ allocaOverlaps (h:prev) r


-- | Check for allocations that overlap in machine code.
checkAllocaOverlap :: BlockLabel -- ^ Label for this block.
                   -> [Ann.AllocaInfo]
                      -- ^ Allocation annotations for this block.
                   -> ModuleVCG ()
checkAllocaOverlap lbl l = do
  case allocaOverlaps [] l of
    [] -> pure ()
    (h:r) -> liftIO $ do
      hPutStrLn stderr $ printf "Error: Overlapping allocations in %s:\n" (show lbl)
      forM_ (h:r) $ \(x,y) -> do
        hPutStrLn stderr $ do
          printf "  Allocation %s overlaps with %s" (show (Ann.allocaName x)) (show (Ann.allocaName y))


-- | Verify a block satisfies its specification.
verifyBlock :: Define
               -- ^ LLVM function that current block is contained
               -- within.
            -> Ann.FunctionAnn -- ^ Annotations for function
            -> BlockLabel -- ^ Label of first block.
            -> L.BasicBlock
            -> ModuleVCG ()
verifyBlock lFun funAnn firstLabel bb = do
  -- Get block label
  let Just lbl = L.bbLabel bb
  -- Get annotations for this block
  blockAnn <-
    case findBlock funAnn lbl of
      Just b -> pure b
      Nothing -> moduleThrow $ "Could not find annotations for block " ++ show lbl
  -- Check allocations fit in stack size.
  forM_ (Ann.blockAllocas blockAnn) $ \a -> do
    when (Ann.allocaBinaryOffset a > Ann.stackSize funAnn) $ do
      moduleThrow $
        printf "Allocation %s has offset in %s that is outside of stack bounds."
          (show (Ann.allocaName a)) (show lbl)
  -- Check allocations do not overlap with each other.
  checkAllocaOverlap lbl (Ann.blockAllocas blockAnn)
  -- Start running verification condition generator.
  runVCGs funAnn firstLabel lbl blockAnn $ do
    blockAddr <- gets mcCurAddr
    -- Add LLVM declarations for all existing allocations.
    forM_ (Ann.blockAllocas blockAnn) $ \a  -> do
      when (Ann.allocaExisting a) $ do
        allocaDeclarations (Ann.allocaName a) (Ann.allocaSize a)

    -- Declare registers for block and function start.
    let blockName = addrName (segoffAddr blockAddr)
    -- Declare memory
    addCommand $ SMT.declareConst (memVar 0) memSort
    -- Declare constant representing where we return to.
    defineVarWithReadValue "return_addr" (varTerm "stack_high") addrSupportedMemType
    -- Declare LLVM arguments in terms of Macaw registers
    when (firstLabel == lbl) $
      defineLLVMArgs blockName (defArgs lFun) x86ArgRegs
    -- Assume preconditions
    do regs <- gets mcCurRegs
       mem  <- gets $ varTerm . memVar . mcMemIndex
       forM_ (Ann.blockPreconditions blockAnn) $ \p -> do
         assume (evalAnnPred regs mem p)
    -- Start processing LLVM statements
    stmtsEq (bbStmts bb)

$(pure [])


-- | Verify a particular function satisfies its specification.
verifyFunction :: Module
               -- ^ LLVM Module
               -> Ann.FunctionAnn
               -- ^ Annotations to add in mapping LLVM module and
               -- memory layout.
               -> ModuleVCG ()
verifyFunction lMod funAnn = do
  modCtx <- ask
  let fnm = Ann.llvmFunName funAnn
  vcgLog $ "Analyzing " ++ fnm

  lFun <-
    case getDefineByName lMod fnm of
      Just f -> pure f
      Nothing -> moduleThrow $ printf "Could not find LLVM function %s in module." fnm

  when (length (defArgs lFun) > length x86ArgRegs) $ do
    moduleThrow $ "Too many arguments."

  case defBody lFun of
    [] -> moduleThrow $ "Expected function to have at least one basic block."
    firstBlock:restBlocks -> do
      let Just entryLabel = bbLabel firstBlock
      firstBlockAnn <-
        case findBlock funAnn entryLabel of
          Just b -> pure b
          Nothing ->
            moduleThrow $ printf "Could not find annotations for LLVM block %%%s." (ppBlock entryLabel)

      let Right addr = getMCAddrOfLLVMFunction (symbolAddrMap modCtx) fnm
      when (toInteger addr /= toInteger (Ann.blockAddr firstBlockAnn)) $ do
        moduleThrow $ "LLVM function " ++ fnm ++ " does not have expected address: " ++ show addr

      -- Verify the blocks.
      forM_ (firstBlock:restBlocks) $ \bb -> do
        verifyBlock lFun funAnn entryLabel bb

$(pure [])

warning :: String -> IO ()
warning msg = do
  hPutStrLn stderr ("Warning: " ++ msg)

fatalError :: String -> IO a
fatalError msg = do
  hPutStrLn stderr msg
  exitFailure

$(pure [])

-- | Read an elf from a binary
readElf :: FilePath -> IO (Elf.Elf 64)
readElf path = do
  contents <- BS.readFile path
  case Elf.parseElf contents of
    Elf.ElfHeaderError _ msg ->
      fatalError msg
    Elf.Elf32Res{} -> do
      fatalError "Expected 64-bit elf file."
    Elf.Elf64Res errs e -> do
      mapM_ (warning . show) errs
      unless (Elf.elfMachine e == Elf.EM_X86_64) $ do
        fatalError "Expected a x86-64 binary"
      unless (Elf.elfOSABI e `elem` [Elf.ELFOSABI_LINUX, Elf.ELFOSABI_SYSV]) $ do
        warning "Expected Linux binary"
      pure e

main :: IO ()
main = do
  (metaCfg, gen) <- withVCGArgs
  e <- readElf $ Ann.binFilePath metaCfg
  let loadOpts = defaultLoadOptions
  (warnings, mem, _entry, symbols) <-
    case resolveElfContents loadOpts e of
      Left err -> do
        hPutStrLn stderr $
          "Could not interpret Elf file " ++ Ann.binFilePath metaCfg ++ ":\n"
          ++ "  " ++ err
        exitFailure
      Right r -> pure r

  forM_ warnings $ \w -> do
    hPutStrLn stderr w

  -- Get LLVM module
  lMod <- do
    res <- parseBitCodeFromFile (Ann.llvmBCFilePath metaCfg)
    case res of
      Left err -> do
        hPutStrLn stderr $ "Could not parse LLVM file: " ++ show err
        exitFailure
      Right m ->
        pure m

  -- Create verification coontext for module.
  errorRef <- newIORef 0
  let modCtx = ModuleVCGContext { moduleMem = mem
                                , symbolAddrMap = Map.fromList
                                                  [ (memSymbolName sym, memSymbolStart sym)
                                                  | sym <- symbols
                                                  ]
                                , writeStderr = True
                                , errorCount = errorRef
                                , proverGen = gen
                                }
  -- Run verification.
  runModuleVCG modCtx $ do
    forM_ (Ann.functions metaCfg) $ \funAnn -> do
      moduleCatch $ verifyFunction lMod funAnn
  errorCnt <- readIORef errorRef
  if errorCnt > 0 then
    hPutStrLn stderr "Errors during verification."
   else
    sessionComplete gen
