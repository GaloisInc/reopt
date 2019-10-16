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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Internal as Aeson
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
import           GHC.IO.Exception (IOErrorType( ResourceVanished ))
import           GHC.Natural
import           GHC.Stack
import           Numeric
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error
import qualified System.Process as P
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
  , proveFalseCallback :: !(HasCallStack => SMT.Term -> String -> IO ())
    -- ^ Invoked when we have a proposition to prove is false for all
    -- interprettions.
    --
    -- The message is provide so the user knows the source of the
    -- check.
  , proveTrueCallback  :: !(HasCallStack => SMT.Term -> String -> IO ())
    -- ^ Invoked when we have a proposition to prove is true for all
    -- interpretations.
    --
    -- The message is provide so the user knows the source of the
    -- check.
  , blockErrorCallback :: String -> IO ()
    -- ^ Report a block error
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
  ModuleVCGContext { moduleAnn :: !Ann.ModuleAnnotations
                     -- ^ Annotations for module.
                   , moduleMem :: !(Memory 64)
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
                   , moduleTypeMap :: !(Map L.Ident L.Type)
                     -- ^ type map for module.
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

-- | Catch a VCG error, print it to the screen and keep going.
moduleCatch :: ModuleVCG () -> ModuleVCG ()
moduleCatch (ModuleVCG m) = ModuleVCG $ ReaderT $ \ctx -> do
  catch (runReaderT m ctx) $ \(ModuleError e) -> do
    when (writeStderr ctx) $ do
      hPutStrLn stderr $ e
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
  , currentBlock :: !BlockLabel
    -- ^ Label for block we are verifying.
  , callbackFns :: !ProverInterface
    -- ^ Functions for interacting with SMT solver.
  , mcBlockEndAddr :: !(MemAddr 64)
    -- ^ The end address of the block.
  , mcBlockMap :: !(Map (MemSegmentOff 64) Ann.MemoryAccessType)
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
    -- ^ Index of last defined memory object.
   , mcEvents :: ![M.Event]
    -- ^ Unprocessed events from last instruction.
  , mcLocalIndex :: !Integer
    -- ^ Index of next local variable for machine code.
  , mcPendingAllocaOffsetMap :: !(Map Ann.LocalIdent Ann.AllocaInfo)
    -- ^ This is a map from allocation names to the offset relative to
    -- the top of the function stack frame.  In this case, we do not
    -- include the 8 bytes storing the return address.  These are
    -- allocas that have not been made when the block starts.
  , llvmInstIndex :: !Int
    -- ^ Index of next LLVM instruction within block to execute
    -- Used for error reporting
  , activeAllocaSet :: !(Set Ann.LocalIdent)
    -- ^ Set of allocation names that are active.
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

$(pure [])

-- | This prepends the LLVM and machine code location information for
-- display to user.
prependLocation :: String -> BlockVCG String
prependLocation msg = do
  thisFun <- asks $ Ann.llvmFunName . curFunAnnotations
  thisBlk <- asks $ ppBlock . currentBlock
  thisInst <- gets $ llvmInstIndex
  addr <- gets $ mcCurAddr
  return $! printf "%s.%s.%d (%s) - %s" thisFun thisBlk thisInst (showsPrec 10 addr "") msg

$(pure [])

-- | Report an error at the given location and stop verification of
-- this block.
fatalBlockError :: String -> BlockVCG a
fatalBlockError msg = do
  annMsg <- prependLocation msg
  liftIO $ hPutStrLn stderr annMsg
  callback <- asks $ blockErrorCallback . callbackFns
  liftIO $ callback msg
  haltBlock

$(pure [])

addCommand :: SMT.Command -> BlockVCG ()
addCommand cmd = do
  prover <- asks callbackFns
  liftIO $ addCommandCallback prover cmd

$(pure [])

-- | @proveTrue p msg@ adds a proof obligation @p@ is true for all
-- interpretations of constants with the message @msg@.
proveTrue :: HasCallStack => SMT.Term -> String -> BlockVCG ()
proveTrue p msg = do
  annMsg <- prependLocation msg
  fns <- asks callbackFns
  liftIO $ proveTrueCallback fns p annMsg
  addCommand $ SMT.assert p

-- | @proveEq x y msg@ add a proof obligation named @msg@ asserting
-- that @x@ equals @y@.
proveEq :: HasCallStack => SMT.Term -> SMT.Term -> String -> BlockVCG ()
proveEq x y msg = do
  fns <- asks callbackFns
  annMsg <- prependLocation msg
  liftIO $ proveFalseCallback fns (SMT.distinct [x,y]) annMsg
  -- Add command for future proofs
  addCommand $ SMT.assert (SMT.eq [x,y])

-- | Add assertion that the propositon is true without requiring it to be proven.
addAssert :: SMT.Term -> BlockVCG ()
addAssert p = addCommand $ SMT.assert p

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
                          -> Map (Some X86Reg) SMT.Term
                          -> IO (RegState X86Reg (Const SMT.Term))
declareAddrStartRegValues prover nm definedRegMap = do
  let initReg :: X86Reg tp -> IO (Const SMT.Term tp)
      initReg reg = do
        let regName = addrStartRegValue nm reg
        case Map.lookup (Some reg) definedRegMap of
          Nothing -> do
            addCommandCallback prover $! SMT.declareFun regName  [] (M.toSMTSort (M.typeRepr reg))
          Just t -> do
            addCommandCallback prover $! SMT.defineFun regName  [] (M.toSMTSort (M.typeRepr reg)) t
        pure $! Const (varTerm regName)
  mkRegStateM initReg

-- | This is the list of callee saved registers.
calleeSavedGPRegs :: [X86Reg (M.BVType 64)]
calleeSavedGPRegs = X86_GP <$> Ann.calleeSavedGPRegs

-- | This is the list of callee saved registers.
x86ArgGPRegs :: [X86Reg (M.BVType 64)]
x86ArgGPRegs = X86_GP <$> Ann.x86ArgGPRegs

fnStartSortedGPRegList :: [X86Reg (M.BVType 64)]
fnStartSortedGPRegList = Set.toList $ Set.fromList $ x86ArgGPRegs ++ calleeSavedGPRegs ++ [RSP]

-- | Return the name of the SMT variable for the register when the
-- function starts.
--
-- This is used by callee saved registers
functionStartRegValue :: IsString a => X86Reg tp -> a
functionStartRegValue reg = fromString $ "fnstart_" <> show reg

-- | Declare an SMT value for each register that defines the value of the
-- register when the block starts execution.
declareFunctionStartRegValues :: ProverInterface -> IO ()
declareFunctionStartRegValues prover = do
  let initReg reg =
        addCommandCallback prover $!
          SMT.declareFun (functionStartRegValue reg) [] (M.toSMTSort (M.typeRepr reg))
  traverse_ initReg fnStartSortedGPRegList

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

memTerm :: Natural -> SMT.Term
memTerm = varTerm . memVar

-- | Create mem write declaration given number of bytes to write
memReadDeclBV :: Natural
              -> SMT.Command
memReadDeclBV w = do
  let nm = "mem_readbv" <> fromString (show (8*w))
  let args = [("m", memSort), ("a", ptrSort)]
  let resSort = SMT.bvSort (8*w)
  SMT.defineFun nm args resSort $
    readBVLE "m" "a" w

$(pure [])

-- | @readFromMCMemory mem addr tp@ reads a value with type @tp@ from
-- address @addr@ in machine code memory @mem@.
--
-- This is a total function, so if you read from invalid memory, it
-- denotes some value, so you need to check whether the address is
-- valid independently.
readFromMemory :: SMT.Term -> SMT.Term -> SupportedMemType -> SMT.Term
readFromMemory mem addr supType =
  SMT.term_app ("mem_read" <> fromString (supportedSuffix supType)) [mem, addr]

$(pure [])

-- | Get current machine code memory
getMCMem :: BlockVCG SMT.Term
getMCMem = gets $ varTerm . memVar . mcMemIndex

$(pure [])

-- | @defineVarFromReadMCMem var addr tp@ reads a value with type @tp@
-- from the address and assigns the value to @var@.
--
-- Note. This function assumes that code has already checked the address is
-- a valid address.
defineVarFromReadMCMem :: Var -> SMT.Term -> SupportedMemType -> BlockVCG ()
defineVarFromReadMCMem valVar addr supType = do
  mem <- getMCMem
  addCommand $ SMT.defineFun valVar [] (supportedSort supType) (readFromMemory mem addr supType)

$(pure [])

-- | Create mem write declaration given number of bytes to write
memWriteDeclBV :: Natural -> SMT.Command
memWriteDeclBV w = do
  let nm = "mem_writebv" <> fromString (show (8*w))
  let argTypes = [("m", memSort), ("a", ptrSort), ("v", SMT.bvSort (8*w))]
  SMT.defineFun nm argTypes memSort (writeBVLE "m" "a" "v" w)

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

-- | @mcWrite addr cnt val@ writes @cnt@ bytes to the machine code
-- memory.
--
-- The value written is the @8*cnt@-length bitvector term @val@.
mcWrite :: SMT.Term -> MemRepr tp -> SMT.Term -> BlockVCG ()
mcWrite addr memType val = do
  -- Get current index for machine code memory
  idx <- gets mcMemIndex
  -- Get suffice for memory write operation.
  suf <- supportedSuffix <$> getSupportedType memType
  -- Define new memory with address updated.
  addCommand $ SMT.defineFun (memVar (idx+1)) [] memSort $
    SMT.term_app ("mem_write" <> fromString suf) [memTerm idx, addr, val]
  -- Increment memory count
  modify' $ \s -> s { mcMemIndex = mcMemIndex s + 1 }

$(pure [])

-- | Name of SMT predicate that holds if all the bytes [addr, addr+sz)` are
-- in the stack and do not overlap with LLVM visible stack.
mcOnlyStackRange :: Text
mcOnlyStackRange = "mc_only_stack_range"

-- | @addc w x y@ returns an expression equal to @bvadd x y@.
--
-- @w@ should be positive.
addc :: Natural -> SMT.Term -> Natural -> SMT.Term
addc _ t 0 = t
addc w t i = SMT.bvadd t [SMT.bvdecimal (toInteger i) w]

-- | @subc w x y@ returns an expression equal to @bvsub x y@.
--
-- @w@ should be positive.
subc :: Natural -> SMT.Term -> Natural -> SMT.Term
subc _ t 0 = t
subc w t i = SMT.bvsub t (SMT.bvdecimal (toInteger i) w)

-- | @mulc w x y@ returns an expression equal to @bvmul x y@.
--
-- @w@ should be positive.
mulc :: Natural -> Natural -> SMT.Term -> SMT.Term
mulc w 0 _ = SMT.bvdecimal 0 w
mulc _ 1 x = x
mulc w c x = SMT.bvmul (SMT.bvdecimal (toInteger c) w) [x]

-- | @defineRangeCheck nm low high@ introduces the definition for a
-- function named @nm@ that takes an address @a@ and size @sz@, and
-- checks that @[a..a+sz)@ is in @[low..high)@ and that @a+sz@ does not overflow.
defineRangeCheck :: Text -> SMT.Term -> SMT.Term -> SMT.Command
defineRangeCheck nm low high = do
  let args = [("a", ptrSort), ("sz", SMT.bvSort 64)]
  SMT.defineFun nm args SMT.boolSort $
    SMT.letBinder [ ("e", SMT.bvadd "a" ["sz"]) ] $
      SMT.and [ SMT.bvule low "a"
              , SMT.bvule "a" "e"
              , SMT.bvule "e" high
              ]

-- | Evaluate a stack is in a range check.
evalRangeCheck :: Text -> SMT.Term -> Natural -> SMT.Term
evalRangeCheck nm a sz =
  SMT.term_app (Builder.fromText nm) [a, SMT.bvdecimal (toInteger sz) 64]

-- | Defines a predicate @(not_in_stack_range a sz)@ that holds if @a + sz@
-- does not overflow and @[a..a+sz)@ does not overlap with the
-- range @[stack_alloc_min..stack_max)@.
--
-- See `mcMemDecls` for details about `stack_alloc_max` and `stack_max`.
defineNotInStackRange :: SMT.Command
defineNotInStackRange = do
  let args = [("a", ptrSort), ("sz", SMT.bvSort 64)]
  SMT.defineFun "not_in_stack_range" args SMT.boolSort $
    SMT.letBinder [ ("e", SMT.bvadd "a" ["sz"]) ] $
      SMT.and [ SMT.bvule "a" "e"
              , SMT.or [ SMT.bvule "e" "stack_alloc_min"
                       , SMT.bvule "stack_max" "a"
                       ]
              ]

-- | A SMT predicate that holds if all the bytes [addr, addr+sz)` are
-- outside the stack.
--
-- Note. This predicate can assume that `sz > 0` and `sz < 2^64`, but still
-- be correct if the computation of `addr+sz` overflows.
notInStackRange :: SMT.Term -> Natural -> SMT.Term
notInStackRange addr sz =
  SMT.term_app "not_in_stack_range" [addr, SMT.bvdecimal (toInteger sz) 64]

-- | @stackHighTerm@ denotes the top of the stack.
stackHighTerm :: SMT.Term
stackHighTerm = functionStartRegValue RSP

-- | @allocaMCBaseEndDecls a@ introduces variables for defining the
-- extent in the machine code stack of an LLVM alloca.
allocaMCBaseEndDecls :: Ann.AllocaInfo -> [SMT.Command]
allocaMCBaseEndDecls a =
  let nm = Ann.allocaIdent a
      base = Ann.allocaBinaryOffset a
      end  = base - Ann.allocaSize a
  -- Define machine code base for allocation.
   in [ SMT.defineFun (allocaMCBaseVar nm) [] ptrSort (subc 64 stackHighTerm base)
      , SMT.defineFun (allocaMCEndVar  nm) [] ptrSort (subc 64 stackHighTerm end)
        -- Introduce predicate to check machine-code addresses.
      , defineRangeCheck (isInMCAlloca nm) (varTerm (allocaMCBaseVar nm)) (varTerm (allocaMCEndVar nm))
      ]

-- | @isPageAligned a sz@ asserts that @a@ is a multiple of @sz@
-- (which must be a power of two.
isPageAligned :: SMT.Term -> Natural -> SMT.Term
isPageAligned a sz
  | sz < 0 = error "isPageAligned must have positive value."
  | otherwise =
    SMT.eq [ SMT.bvand a [SMT.bvhexadecimal (toInteger (sz-1)) 64]
           , SMT.bvdecimal 0 64
           ]

-- | Variable indicating a range is between `stack_guard_min` and
-- `stack_max`.
--
-- See `mcMemDecls` for discussion about these variables.
onStack :: IsString a => a
onStack = "on_stack"

-- | @mcMemDecls bytesAbove allocas@ adds declarations about the memory.
--
-- We assume that both LLVM and machine code have a set of memory
-- devoted to the stack.  This stack is a contiguous set of bytes
-- defined by three variables with two page-aligned subregions:
--
-- The upper part of the region is the stack itself, and although not
-- all pages are necessarily allocated, we assume the pages will be
-- allocated when faults occur.
--
-- The lower part of the region is a guard region with a fixed number
-- of bytes in size.  Attempts to read/write memory in this region
-- should result in segmentation faults.
--
-- Because we have to prove that for each LLVM trace, there exists
-- an equivalent machine code trace, we can safely assume that the stack
-- region addresses are the same in both machine code and LLVM.
--
-- These regions are defined via three variables and page size
-- and guard page count constants taken from the platform description.
--
-- stack_guard_min: The lower bound of the guard region.
--
-- stack_alloc_min: The lower bound of the allocated region.
--
-- stack_max: The upper bound of the stack.
--
-- We make the following assumptions:
--
-- * stack_guard_min, stack_alloc_min and stack_max are all page
--   aligned.
-- * stack_alloc_min = stack_guard_min + guard_page_count * page_size
-- * stack_alloc_min < stack_max
--
-- Note. This assumes X86 registers are already declared.
mcMemDecls :: Natural
              -- ^ The size of pages for this platform.
              -- Should be a power of two.
           -> Natural
           -- ^ The number of guard pages for the stack.
           -> [Ann.AllocaInfo]
              -- ^ Allocations
           -> [SMT.Command]
mcMemDecls pageSize guardPageCount allocas
  | guardPageCount == 0 = error "guardPageCount must be positive."
  | otherwise =
  (let guardSize = pageSize * guardPageCount
    in [ SMT.declareConst "stack_alloc_min" (SMT.bvSort 64)
       , SMT.assert $ isPageAligned "stack_alloc_min" pageSize
       , SMT.assert $ SMT.bvult (SMT.bvdecimal (toInteger guardSize) 64) "stack_alloc_min"

       , SMT.defineFun "stack_guard_min" [] (SMT.bvSort 64) $
           subc 64 "stack_alloc_min" guardSize
       , SMT.assert $ SMT.bvult "stack_guard_min" "stack_alloc_min"

         -- Declare the upper bound on stack address.
       , SMT.declareConst "stack_max" (SMT.bvSort 64)
       , SMT.assert $ isPageAligned "stack_max" pageSize
         -- Assert stack_alloc_min < stack_max
       , SMT.assert $ SMT.bvult "stack_alloc_min" "stack_max"

         -- Assert RSP is between stack_alloc_min and stack_max - return address size
       , SMT.assert $ SMT.bvule "stack_alloc_min" stackHighTerm
       , SMT.assert $ SMT.bvule stackHighTerm (subc 64 "stack_max" 8)

         -- Define check to assert stack is in given range
       , defineRangeCheck onStack "stack_guard_min" "stack_max"
       -- Declare not in stack that asserts a range is not on the stack.
       , defineNotInStackRange
       -- Assert that stack pointer is at least 8 below stack high
       , SMT.assert $ SMT.bvult stackHighTerm (subc 64 "stack_max" 8)
       -- High water stack pointer includes 8 bytes for return address.
       -- The return address top must be aligned to a 16-byte boundary.
       -- This is done by asserting the 4 low-order bits of fnstart_rsp are 8.
       , SMT.assert $ SMT.eq [ SMT.extract 3 0 stackHighTerm, SMT.bvdecimal 8 4]
       ])
  ++ concatMap allocaMCBaseEndDecls allocas
  -- Declare mcOnlyStackRange
  ++ [ SMT.defineFun mcOnlyStackRange [("a", ptrSort), ("sz", SMT.bvSort 64)] SMT.boolSort $
         SMT.letBinder [ ("e", SMT.bvadd "a" ["sz"]) ] $
           SMT.and $ [ SMT.term_app (Builder.fromText "on_stack") ["a", "sz"] ]
                     ++ [ isDisjoint ("a", "e") (allocaMCBaseVar nm, allocaMCEndVar nm)
                        | a <- allocas
                        , let nm = Ann.allocaIdent a
                        ]
     ]

------------------------------------------------------------------------

-- | When that a feature is missing.
missingFeature :: String -> BlockVCG ()
missingFeature msg = liftIO $ hPutStrLn stderr $ "TODO: " ++ msg

-- | Identifies the LLVM base address of an allocation.
allocaLLVMBaseVar :: (IsString a, Monoid a) => Ann.LocalIdent -> a
allocaLLVMBaseVar = \(Ann.LocalIdent nm) -> "alloca_" <> fromString (Text.unpack nm) <> "_llvm_base"

-- | Identifies the LLVM end address of an allocation.
allocaLLVMEndVar :: (IsString a, Monoid a) => Ann.LocalIdent -> a
allocaLLVMEndVar = \(Ann.LocalIdent nm) -> "alloca_" <> fromString (Text.unpack nm) <> "_llvm_end"

-- | Identifies the machine code base address of an allocation.
allocaMCBaseVar :: (IsString a, Monoid a) => Ann.LocalIdent -> a
allocaMCBaseVar = \(Ann.LocalIdent nm) -> "alloca_" <> fromString (Text.unpack nm) <> "_mc_base"

-- | Name of a predicate that checks if a range [start, start+size)]
-- is in the range of an allocation for LLVM addresses.
isInLLVMAlloca :: Ann.LocalIdent -> Text
isInLLVMAlloca (Ann.LocalIdent nm) = mconcat ["llvmaddr_in_alloca_", nm]

-- | Name of a predicate that checks if a range [start, start+size)]
-- is in the range of an allocation for machine code addresses.
isInMCAlloca :: Ann.LocalIdent -> Text
isInMCAlloca (Ann.LocalIdent nm) = mconcat ["mcaddr_in_alloca_", nm]

-- | Identifies the LLVM end address of an allocation.
allocaMCEndVar :: (IsString a, Monoid a) => Ann.LocalIdent -> a
allocaMCEndVar (Ann.LocalIdent nm)  = "alloca_" <> fromString (Text.unpack nm) <> "_mc_end"

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
assumeLLVMDisjoint :: Range -> Ann.LocalIdent -> SMT.Term
assumeLLVMDisjoint r nm = isDisjoint r (allocaLLVMBaseVar nm, allocaLLVMEndVar nm)

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

-- | Return true if the first address is always less than second.
addrLt :: MemAddr 64 -> MemAddr 64 -> Bool
addrLt x y = addrBase x == addrBase y && addrOffset x < addrOffset y

mcNextAddr :: BlockVCGState -> MemAddr 64
mcNextAddr s = incAddr (toInteger (mcCurSize s)) (segoffAddr (mcCurAddr s))

-- | Get next events
getNextEvents :: HasCallStack => BlockVCG ()
getNextEvents = do
  ctx <- ask
  s <- get
  let addr = mcNextAddr s
  when (not (addrLt addr (mcBlockEndAddr ctx))) $ do
    error $ "Unexpected end of machine code events."
  let mem = moduleMem (mcModuleVCGContext ctx)
  let Just addrSegOff = asSegmentOff mem addr
  let loc = ExploreLoc { loc_ip = addrSegOff
                       , loc_x87_top = mcX87Top s
                       , loc_df_flag = mcDF s
                       }
  (events, nextIdx, sz) <-
    case M.instructionEvents (mcBlockMap ctx) (mcCurRegs s) (mcLocalIndex s) loc of
      Left e -> fatalBlockError e
      Right p -> pure p
  -- Update local index and next addr
  put $! s { mcLocalIndex = nextIdx
           , mcCurAddr  = addrSegOff
           , mcCurSize  = sz
           }
  -- Update events
  modify $ \t -> t { mcEvents = events }

-- | Set machine code registers from reg state.
setMCRegs :: M.EvalContext
          -> RegState (ArchReg X86_64) (Value X86_64 ids)
          -> BlockVCG ()
setMCRegs ectx regs = do
  topVal <- case regs^.boundValue X87_TopReg of
              BVValue _w i | 0 <= i, i <= 7 -> pure $! fromInteger i
              _ -> error "Unexpected X87_TOP value"
  dfVal <- case regs^.boundValue DF of
             BoolValue b -> pure b
             _ -> error "Unexpected direction flag"
  modify' $ \s -> s { mcX87Top = topVal
                    , mcDF = dfVal
                    , mcCurRegs = fmapF (Const . M.primEval ectx) regs
                    }


-- | Execute the machine-code only events that occur before jumping to the given address
execMCOnlyEvents :: HasCallStack => MemAddr 64 -> BlockVCG ()
execMCOnlyEvents endAddr = do
  evts <- gets mcEvents
  case evts of
    M.CmdEvent cmd:mevs -> do
      addCommand cmd
      modify $ \s -> s { mcEvents = mevs }
      execMCOnlyEvents endAddr
    M.WarningEvent msg:mevs -> do
      liftIO $ hPutStrLn stderr msg
      modify $ \s -> s { mcEvents = mevs }
      execMCOnlyEvents endAddr
    M.MCOnlyStackReadEvent mcAddr tp macawValVar:mevs -> do
      -- TODO: Fix this to the following

      -- A MCOnlyStack read means the machine code reads memory, but
      -- the stack does not.
      --
      -- We currently check that these reads only access the stack as
      -- the only current use of these annotations is to mark register
      -- spills, return address read/writes, and frame pointer/callee saved
      -- register saves/restores.
      --
      -- Checking this is on the stack also ensures there are no side effects
      -- from mem-mapped IO reads since the stack should not be mem-mapped IO.
      do thisIP <- gets mcCurAddr
         proveTrue (evalRangeCheck onStack mcAddr (memReprBytes tp)) $
           printf "Machine code read at %s is not within stack space." (show thisIP)
      -- Define value from reading Macaw heap
      supType <- getSupportedType tp
      defineVarFromReadMCMem macawValVar mcAddr supType
      -- Process future events.
      modify $ \s -> s { mcEvents = mevs }
      execMCOnlyEvents endAddr
    -- Every LLVM write should have a machine code write (but not
    -- necessarily vice versa), we pattern match on machine code
    -- writes.
    M.MCOnlyStackWriteEvent mcAddr tp macawVal:mevs -> do
      -- We need to assert that this write will not be visible to LLVM.
      do addr <- gets mcCurAddr
         proveTrue (evalRangeCheck mcOnlyStackRange mcAddr (memReprBytes tp)) $
           printf "Machine code write at %s is in unreserved stack space." (show addr)
      -- Update stack with write.
      mcWrite mcAddr tp macawVal
      -- Process next events
      modify $ \s -> s { mcEvents = mevs }
      execMCOnlyEvents endAddr
    -- This checks to see if the next instruction jumps to the next ip,
    -- and if so it runs it.
    (M.FetchAndExecuteEvent ectx regs:r) -> do
      when (not (null r)) $ do
        error "MC event after fetch and execute"
      modify $ \s -> s { mcEvents = [] }
      -- Update registers
      setMCRegs ectx regs
      -- Process next events
      nextAddr <- gets mcNextAddr
      case valueAsMemAddr (regs^.boundValue X86_IP) of
        Just ipAddr | ipAddr == nextAddr && addrLt nextAddr endAddr -> do
                        getNextEvents
                        execMCOnlyEvents endAddr
        _ -> do
          pure ()
    [] -> do
      nextAddr <- gets mcNextAddr
      when (addrLt nextAddr endAddr) $ do
        getNextEvents
        execMCOnlyEvents endAddr
    _:_ -> do
      pure ()

-- | Get the next MC event that could interact with LLVM.
popMCEvent :: HasCallStack => BlockVCG M.Event
popMCEvent = do
  endAddr <- asks mcBlockEndAddr
  execMCOnlyEvents endAddr
  evts <- gets mcEvents
  case evts of
    [] -> do
      error "Reached end of block"
    (h:r) -> do
      modify $ \s -> s { mcEvents = r }
      pure h

-- | Move to end of current block.
mcExecuteToEnd :: HasCallStack => BlockVCG ()
mcExecuteToEnd = do
  endAddr <- asks mcBlockEndAddr
  execMCOnlyEvents endAddr
  evts <- gets mcEvents
  case evts of
    [] -> do
      pure ()
    (h:_) -> do
      error $ "Expecting end of block instead of " ++ show h

-- | Check direction flag is clear.
--
-- This must be checked on calls and returns.
checkDirectionFlagClear :: BlockVCG ()
checkDirectionFlagClear = do
  df <- gets mcDF
  when df $ error "Direction flag must be clear."

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

llvmInvoke :: HasCallStack
           => Bool
           -> L.Symbol
           -> [Typed (Value' BlockLabel) ]
           -> (Maybe (Ident, Type))
           -> BlockVCG ()
llvmInvoke isTailCall fsym args lRet = do
  when isTailCall $ error "Tail calls are not yet supported."
  -- Run the machine until we reach a call (which wil terminate the block)
  execMCOnlyEvents =<< asks mcBlockEndAddr
  -- Get current registers
  regs <- gets mcCurRegs
  -- Evaluate arguments.
  lArgs <- traverse evalTyped args
  let mRegIP = getConst $ regs ^. boundValue X86_IP
  assertFnNameEq fsym mRegIP
  -- Verify that the arguments should be same.  Note: Here we take the
  -- number of arguments from LLVM side, since the number of arguments
  -- in Macaw side seems not explicit.  Also assuming that the # of
  -- arguments of LLVM side is less or equal than six.
  when (length lArgs > length x86ArgGPRegs) $ do
    error $ "Too many arguments."
  -- Check LLVM and machine code arguments match
  do let compareArg :: SMT.Term -> X86Reg (M.BVType 64) -> BlockVCG ()
         compareArg la reg = do
           let Const ma = regs^.boundValue reg
           proveEq la ma "Register matches LLVM"
     zipWithM_ compareArg lArgs x86ArgGPRegs

  -- Check direction flag is clear
  checkDirectionFlagClear

  -- Get address of next instruction as an SMT term.
  nextInsnAddr <- gets $ mcNextAddr

  -- Get value of RSP at the function call.
  let curRSP :: SMT.Term
      curRSP = getConst (regs^.boundValue RSP)

  -- TODO: So far we have a function call.
  --
  -- We cannot check that the LLVM stack address is the same as the machine
  -- code one, so this we skip this.
  --
  -- We check that the return address matches the address of the
  -- next instruction so that the return in LLVM matches the return
  -- address in machine code.

  -- Check check pointer is valid and we saved return address on call.
  do -- Get value stored at return address
     mem <- gets  $ varTerm . memVar . mcMemIndex
     -- Check stored return value matches next instruction
     proveEq (readFromMemory mem curRSP addrSupportedMemType) (M.evalMemAddr nextInsnAddr)
       "Check return address matches next instruction."


  -- Add 8 to RSP for post-call value to represent poping the stack pointer.
  let postCallRSP :: SMT.Term
      postCallRSP = addc 64 (getConst (regs^.boundValue RSP)) 8

  -- Create registers for instruction after call.
  --
  -- This ensures that callee saved registers and the stack pointer
  -- are preserved, but nothing is assumed about other registers.
  newRegs <- do
    prover <- asks callbackFns
    let calleeRegValues =
          [ (Some r, getConst $ regs^.boundValue r)
          | r <- calleeSavedGPRegs
          ]
          ++ [ (Some RSP, postCallRSP)
             , (Some DF, SMT.false)
             , (Some X87_TopReg, SMT.bvdecimal 7 3)
             ]
    liftIO $
      declareAddrStartRegValues prover (addrName nextInsnAddr) (Map.fromList calleeRegValues)
  modify $ \s -> s { mcX87Top = 7
                   , mcDF = False
                   , mcCurRegs = newRegs
                   }

  -- Update machine code memory to post-call memory.
  do -- Get current index for machine code memory
     idx <- gets mcMemIndex
     -- Define new memory for post-call state
     addCommand $ SMT.declareConst (memVar (idx+1)) memSort
     -- Assert the memory from @[postCallRSP, rsp0+8)@ has not changed.
     addAssert $ SMT.term_app "eqrange" [ memTerm (idx+1)
                                        , memTerm idx
                                        , postCallRSP
                                        , addc 64 stackHighTerm 7
                                        ]
     -- Increment memory count
     modify' $ \s -> s { mcMemIndex = mcMemIndex s + 1 }

  -- If LLVM side has a return value, then we define the LLVM event in
  -- terms of the value bound to RAX for the rest of the program.
  case lRet of
    Just (llvmIdent, tp) -> do
      (mRetVal, smtSort) <-
        case tp of
          PtrTo _ ->
            -- Returned pointers can be assumed to be on heap, so we
            -- can assume they are equal.
            pure (getConst $ newRegs^.boundValue RAX, SMT.bvSort 64)
          PrimType (Integer 64) ->
            pure (getConst $ newRegs^.boundValue RAX, SMT.bvSort 64)
          _ ->  error $ "TODO: Add support for return type " ++ show tp
      addCommand $ SMT.defineFun (identVar llvmIdent) [] smtSort  mRetVal
    Nothing -> pure ()

-- | Add the LLVM declarations for an allocation.
allocaDeclarations :: Ann.LocalIdent
                   -> Natural-- ^ Size as a 64-bit unsigned bitector.
                   -> BlockVCG ()
allocaDeclarations nm szNat = do
  -- Get used allocas
  used <- gets activeAllocaSet
  -- Check that alloca name is not in use.
  when (Set.member nm used) $ error $ show nm ++ " is already used an allocation."
  -- Add alloca name to active set.
  modify' $ \s -> s { activeAllocaSet = Set.insert nm used }
  -- Declare LLVM alloca base
  addCommand $ SMT.declareConst (allocaLLVMBaseVar nm) ptrSort
  -- Declare LLVM alloca base
  -- Assert alloca base is not too large.
  addAssert $ SMT.bvule (allocaLLVMBaseVar nm)
                        (SMT.bvdecimal (- (toInteger (szNat + 1))) 64)
  addCommand $ SMT.defineFun (allocaLLVMEndVar nm) [] ptrSort $
    addc 64 (allocaLLVMBaseVar nm) szNat
  -- Introduce predicate to check LLVM addresses.
  addCommand $ defineRangeCheck (isInLLVMAlloca nm) (allocaLLVMBaseVar nm) (allocaLLVMEndVar nm)
  -- Add assumption that LLVM allocation does not overlap with
  -- existing allocations.
  mapM_ (addAssert . assumeLLVMDisjoint (allocaLLVMBaseVar nm, allocaLLVMEndVar nm)) used
  -- Define register alloca is returned to.
  addCommand $ SMT.defineFun (llvmVar (Ann.allocaNameText nm)) [] ptrSort (allocaLLVMBaseVar nm)


-- | This updates the state for an LLVM allocation
llvmAlloca :: HasCallStack
           => Ident -- ^ Identifier to assign this to.
           -> Type -- ^ Type of elements
           -> Maybe (Typed (Value' BlockLabel)) -- ^ Number of elements (Nothing = 1)
           -> Maybe Int -- ^ Required alignment
           -> BlockVCG ()
llvmAlloca (Ident nm0) ty eltCount _malign = do
  eltSize <-
    case ty of
      PrimType (Integer i)
        | i .&. 0x7 == 0, i > 0 ->
            pure $ (fromIntegral i `shiftR` 3 :: Natural)
      PtrTo _ ->
        pure 8
      _ ->
        fatalBlockError $ "Unexpected type " ++ show (L.ppType ty)
  let nm = Ann.LocalIdent (Text.pack nm0)

  allocaMap <- gets $ mcPendingAllocaOffsetMap
  a <-
    case Map.lookup nm allocaMap of
      Nothing -> do
        fnm <- asks $  Ann.llvmFunName . curFunAnnotations
        bnm <- asks $ ppBlock . currentBlock
        fatalBlockError $ printf "%s.%s: Missing annotation on alloca %s." fnm bnm (show nm)
      Just a -> pure a

  -- Delete this from pending allocations
  modify $ \s -> s { mcPendingAllocaOffsetMap = Map.delete nm allocaMap }

  -- Check the size of LLVM allocation matches annotations.
  case eltCount of
    Nothing -> do
      when (eltSize /= Ann.allocaSize a) $
        fatalBlockError $
           printf "Allocation size at %s must match specification %s."
             nm0 (show (Ann.allocaSize a))
    Just (Typed (PrimType (Integer w)) i) | w >= 1 -> do
       cntExpr <- primEval (PrimType (Integer w)) i
       let wn = fromIntegral w
       proveEq (mulc wn eltSize cntExpr) (SMT.bvdecimal (toInteger (Ann.allocaSize a)) wn) $
         printf "Allocation size at %s must match specification %s."
                nm0 (show (Ann.allocaSize a))
    Just (Typed itp _) -> do
      fatalBlockError $ "Unexpected allocation count type " ++ show (L.ppType itp)
  -- Create declarations for alloca.
  allocaDeclarations nm (Ann.allocaSize a)

$(pure [])

-- | Returns ABI byte alignment constraint in bytes.
memTypeAlign :: Set L.Ident -> Map L.Ident L.Type -> L.Type -> Natural
memTypeAlign prevIdents typeMap tp =
  case tp of
    L.PrimType ptp ->
      case ptp of
        _ -> error $ "Alignment of " ++ show tp ++ " is not yet set."
    L.Alias nm
      | Set.member nm prevIdents ->
          error $ "Loop in type aliases."
      | otherwise ->
          case Map.lookup nm typeMap of
            Nothing -> error $ "Could not find type " ++ show nm
            Just utp ->
              memTypeAlign (Set.insert nm prevIdents) typeMap utp
    _ ->
      error $ "Alignment of " ++ show tp ++ " is not yet set."

$(pure [])

-- | Process a LLVM load statement.
llvmLoad :: HasCallStack
         => Ident -- ^ Variable to assign load value to.
         -> Value' BlockLabel
            -- ^ Address being loaded
         -> L.Type -- ^ LLVM type for load
         -> Maybe L.Align -- ^ Alignment
         -> BlockVCG ()
llvmLoad ident src llvmType  malign = do
  llvmAddr <- primEval (PtrTo llvmType) src
  llvmAlign <- do
    let a0 = fromMaybe 0 malign
    if a0 < 0 then
      error $ "alignment must be non-negative."
     else if a0 == 0 then do
      typeMap <- asks $ moduleTypeMap . mcModuleVCGContext
      pure $ memTypeAlign Set.empty typeMap llvmType
     else do
      when ((a0 - 1) .&. a0 /= 0) $ do
        error $ printf "Alignment %s is not a power of 2." (show a0)
      pure $ fromIntegral a0

  when (llvmAlign > 1) $ do
    liftIO $ hPutStrLn stderr $ printf "Warning: LLVM alignment of %s is unchecked." (show llvmAlign)

  mevt <- popMCEvent
  case mevt of
    M.JointStackReadEvent mcAddr mcType macawValVar allocName -> do
      -- Check LLVM type and machine code types are equivalent.
      unless (typeCompat llvmType mcType) $ do
        fatalBlockError "Incompatible LLVM and machine code types."
      let sz = memReprBytes mcType
      -- Check alloca is defined
      do used <- gets $ activeAllocaSet
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
      defineVarFromReadMCMem macawValVar mcAddr supType

      -- Define LLVM value in terms of Macaw value.
      addCommand $ SMT.defineFun (identVar ident) [] (supportedSort supType) (varTerm macawValVar)
    M.NonStackReadEvent mcAddr mcType macawValVar -> do
      -- Check LLVM type and machine code types are equivalent.
      unless (typeCompat llvmType mcType) $ do
        fatalBlockError "Incompatible LLVM and machine code types."
      -- Assert addresses are the same
      proveEq mcAddr llvmAddr
        ("Machine code heap load address matches expected from LLVM")
      -- Add that macaw points to the heap
      do addr <- gets mcCurAddr
         proveTrue (notInStackRange mcAddr (memReprBytes mcType))
           (printf "Read from heap at %s is valid." (show addr))
      -- Define value from reading Macaw heap
      supType <- getSupportedType mcType
      defineVarFromReadMCMem macawValVar mcAddr supType
      -- Define LLVM value returned in terms of macaw value
      addCommand $ SMT.defineFun (identVar ident) [] (supportedSort supType) (varTerm macawValVar)
    _ -> do
      fatalBlockError "Expected a machine code load event."

-- | Handle an LLVM store.
llvmStore :: HasCallStack
          => SMT.Term -- ^ Address
          -> L.Type -- ^ LLVM type
          -> SMT.Term -- ^ Value written
          -> BlockVCG ()
llvmStore llvmAddr llvmType llvmVal = do
  mevt <- popMCEvent
  case mevt of
    M.JointStackWriteEvent mcAddr mcType mcVal allocName -> do
      -- Check the number of bytes written are the same.
      unless (typeCompat llvmType mcType) $ do
        fatalBlockError $ "Machine code and LLVM writes have incompatible types:\n"
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
                (printf "Check machine code write is in %s alloca." (show allocName))
      -- Prove: llvmAddr - llvmAllocaBase = mcAddr - mcAllocaBase
      let llvmOffset = SMT.bvsub llvmAddr llvmAllocaBase
      let mcOffset   = SMT.bvsub   mcAddr   mcAllocaBase
      proveEq llvmOffset mcOffset "LLVM and machine code write to same allocation offset."
      -- Assert values are equal
      thisIP <- gets mcCurAddr
      proveEq llvmVal mcVal $
        (printf "Value written at addr %s equals LLVM value." (show thisIP))
    M.NonStackWriteEvent _mcAddr mcType mcVal -> do
      -- Check types agree.
      unless (typeCompat llvmType mcType) $ do
        error "Macaw and LLVM writes have different types."
      missingFeature "Assert machine code and llvm heap write addresses are equal."
      -- Assert values are equal
      proveEq llvmVal mcVal
        ("Machine code heap store matches expected from LLVM")
    _ -> do
      error "llvmStore: Expected a Macaw heap or joint stack write event."

llvmReturn :: HasCallStack => Maybe (Typed (Value' BlockLabel)) -> BlockVCG ()
llvmReturn mlret = do
  mcExecuteToEnd
  -- Get register values after return.
  regs <- gets mcCurRegs
  -- Assert the IP after the fetch and execute is the return address
  proveEq (getConst (regs^.boundValue X86_IP)) (varTerm "return_addr")
    "Return address matches entry value."

  -- Assert the stack height at the return is just above the return
  -- address pointer.
  proveEq (getConst (regs^.boundValue RSP)) (addc 64 stackHighTerm 8)
    "Stack height at return matches init."

  checkDirectionFlagClear

  do forM_ calleeSavedGPRegs $ \r -> do
       proveEq (getConst (regs^.boundValue r)) (functionStartRegValue r)
          (printf "Value of %s at return is preserved." (show r))

--  We do not support changing any of these:
--  missingFeature "Processor is in x87 (as opposed to MMX mode)"
--  missingFeature "MXCSR control bits must be preserved."
--  missingFeature "X87 control word must be preserved."

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
initBlockRegValues :: Ann.ReachableBlockAnn -> [(Some X86Reg, SMT.Term)]
initBlockRegValues blockAnn =
  [ (Some X86_IP,     SMT.bvhexadecimal (toInteger (Ann.blockAddr blockAnn)) 64)
  , (Some X87_TopReg, SMT.bvdecimal (toInteger (Ann.blockX87Top blockAnn)) 3)
  , (Some DF,         if Ann.blockDFFlag blockAnn then SMT.true else SMT.false)
  ]

$(pure [])

-- | Evaluate a annoatation expression to construct an SMT term.
evalPrecondition :: RegState X86Reg (Const SMT.Term)
                 -> SMT.Term -- ^ Current state of memory
                 -> Ann.Expr Ann.BlockVar
                 -> SMT.Term
evalPrecondition regs mem e = do
  let r = evalPrecondition regs mem
  case e of
    Ann.Eq x y -> SMT.eq [r x, r y]
    Ann.BVSub x y -> SMT.bvsub (r x) (r y)
    Ann.BVDecimal x y -> SMT.bvdecimal (toInteger x) y
    Ann.Var v ->
      case v of
        Ann.StackHigh -> stackHighTerm
        Ann.InitGPReg64 reg -> getConst $ regs^.boundValue (X86_GP reg)
        Ann.FnStartGPReg64 reg ->
          functionStartRegValue (X86_GP reg)
        Ann.MCStack addr bitCount ->
          readFromMemory mem (r addr) (supportedBVMemType (bitCount `shiftR` 3))

$(pure [])

-- | @verifyBlockPreconditions f lbl@ verifies the preconditions for block @lbl@
-- are satisfied in the current state.
--
-- The function @f@ is applied to each predicate before verification,
-- and allows us to conditionally validate some of the preconditions.
verifyBlockPreconditions :: String
                         -> (SMT.Term -> SMT.Term)
                         -> BlockLabel -- ^ Label of block we are jumping to.
                         -> BlockVCG ()
verifyBlockPreconditions prefix f lbl = do
  fnAnn <- asks $ curFunAnnotations
  case findBlock fnAnn lbl of
    Nothing -> do
      fatalBlockError $
        printf "Target block %s lacks annotations." (ppBlock lbl)
    Just Ann.UnreachableBlock ->
      proveTrue (f SMT.false) $
      printf "Target block %s is unreachable." (ppBlock lbl)
    Just (Ann.ReachableBlock tgtBlockAnn) -> do
      firstLabel <- asks firstBlockLabel

      when (lbl == firstLabel) $ fail "Do not support jumping to first label in function."

      -- Get current registers
      regs <- gets mcCurRegs
      -- Check initialized register values
      forM_ (initBlockRegValues tgtBlockAnn) $ \(Some r, expected) -> do
        -- Get register values
        let mcValue = getConst $ regs ^. boundValue r
        proveTrue (f (SMT.eq [expected, mcValue])) $ printf "Checking %s register %s." prefix (show r)

      mem  <- getMCMem
      -- Check preconditions
      forM_ (Ann.blockPreconditions tgtBlockAnn) $ \p -> do
        proveTrue (f (evalPrecondition regs mem p)) $ printf "Checking %s precondition." prefix

-- | Construct SMT sort from type and report error if this fails.
coerceToSMTSort :: Type -> BlockVCG SMT.Sort
coerceToSMTSort ty = do
  case asSMTSort ty of
    Just tp -> pure tp
    Nothing -> error $ "Unexpected type " ++ show (L.ppType ty)

-- | Process LLVM and macaw events to ensure they are equivalent.
--
-- This returns True if we should keep executing as we are not at end of block.
stepNextStmt :: HasCallStack
             => L.Stmt
             -> BlockVCG Bool
stepNextStmt stmt@(L.Result ident inst _mds) = do
  setCurrentLLVMStmt stmt
  case inst of
    Alloca ty eltCount malign -> do
      llvmAlloca ident ty eltCount malign
      pure True
    Arith lop (Typed lty lhs) rhs -> do
      tp <- coerceToSMTSort lty
      lhsv   <- primEval lty lhs
      rhsv   <- primEval lty rhs
      defineTerm ident tp $ arithOpFunc lop lhsv rhsv
      pure True
    Call isTailCall retty f args -> do
      -- Evaluate function
      fSym <- case f of
                ValSymbol s -> pure s
                _ -> fail $ "VCG currently only supports direct calls."
      -- Add invoke event
      llvmInvoke isTailCall fSym args (Just (ident, retty))
      pure True
    Conv convOp (Typed inputType primVal) resultType -> do
      smtResultType <- coerceToSMTSort resultType
      val <- primEval inputType primVal
      result <-
        case convOp of
          PtrToInt -> do
            case inputType of
              PtrTo{} -> pure ()
              _ -> error "ptrtoint expected a pointer."
            when (resultType /= PrimType (Integer 64)) $ do
              error "ptrtoint expected a i64 result."
            pure val
          IntToPtr -> do
            when (inputType /= PrimType (Integer 64)) $ do
              error "inttoptr expected a i64 input type."
            case resultType of
              PtrTo{} -> pure ()
              _ -> error "inttoptr expected a pointer result."
            pure val
          _ -> do
            error $ "Do not yet support " ++ show (L.ppConvOp convOp)
      defineTerm ident smtResultType result
      pure True
    ICmp lop (Typed lty lhs) rhs -> do
      case lty of
        PrimType (Integer w)
          | w > 0 -> do
              pure ()
        _ -> do
          error "ICmp given unexpected type."
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
      pure True
    Load (Typed (PtrTo lty) src) ord malign -> do
      when (isJust ord) $ do
        error $ "Do not yet support atomic ordering."
      llvmLoad ident src lty malign
      pure True
    _ -> do
      error $ "stepNextStmt: unsupported instruction: " ++ show inst
stepNextStmt stmt@(L.Effect instr _mds) = do
  setCurrentLLVMStmt stmt
  case instr of
    Store llvmVal llvmPtr _ordering _align -> do
      addrTerm <- evalTyped llvmPtr
      valTerm  <- evalTyped llvmVal
      llvmStore addrTerm (typedType llvmVal) valTerm
      pure True
    Br (Typed _ty cnd) tlbl flbl -> do
      mcExecuteToEnd
      -- Get condition
      cndTerm <- primEval (PrimType (Integer 1)) cnd
      let c = SMT.eq [cndTerm, SMT.bvdecimal 1 1]
      -- Verify block preconditions.
      verifyBlockPreconditions "true branch"  (SMT.implies [c])         tlbl
      verifyBlockPreconditions "false branch" (SMT.implies [SMT.not c]) flbl
      pure False
    Jump lbl -> do
      mcExecuteToEnd
      -- Verify block preconditions
      verifyBlockPreconditions "jump" id lbl
      pure False
    Ret retVal -> do
      llvmReturn (Just retVal)
      pure False
    RetVoid -> do
      llvmReturn Nothing
      pure False
    _ -> error "Unsupported instruction."

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
interactiveVerifyGoal :: HasCallStack
                      => InteractiveContext -- ^ Context for verifying goals
                      -> SMT.Term
                         -- ^ Negation of goal to verify
                      -> String
                         -- ^ Name of proposition for reporting purposes.
                      -> IO ()
interactiveVerifyGoal ictx negGoal propName = do
  let annFile = ictxAnnFile ictx
  let funName = ictxFunName ictx
  let lbl = ictxBlockLabel ictx
  let cmdHandle = ictxCmdHandle ictx
  let respHandle = ictxRespHandle ictx

  cnt <- readIORef (ictxBlockGoalCounter ictx)

  modifyIORef' (ictxAllGoalCounter ictx)      (+1)
  modifyIORef' (ictxBlockGoalCounter ictx)    (+1)


  let fname = standaloneGoalFilename funName lbl cnt
  hPutStrLn stderr $ printf "Verify: %s" propName
  writeCommand cmdHandle $ SMT.checkSatAssuming [negGoal]
  hFlush cmdHandle
  asyncResp <- ASync.async (SMTP.readCheckSatResponse respHandle)
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
      exitFailure
    (SMTP.CheckSatError msg) -> do
      hPutStrLn stderr $ "Verification failed"
      hPutStrLn stderr $ "The SMT solver returned the following message after check-sat-assuming:"
      hPutStrLn stderr ""
      hPutStrLn stderr $ "  " ++ msg
      hPutStrLn stderr ""
      hPutStrLn stderr $ "This behavior likely reflects a bug in reopt-vcg:"
      hPutStrLn stderr $ prettyCallStack callStack
      hPutStrLn stderr ""
      hPutStrLn stderr $ printf "To see output, run `reopt-vcg %s --export <dir>`." annFile
      hPutStrLn stderr $ "The result will be stored in " ++ fname
      exitFailure

newInteractiveSession :: HasCallStack
                      => FilePath -- ^ Path for annotations
                      -> String -- ^ Command line for SMT solver
                      -> IORef Natural -- ^ Counter for each goal
                      -> IORef Natural -- ^ Counter for each verified goal.
                      -> IORef Natural -- ^ Counter for errors
                      -> FunctionName -- ^ Name of function
                      -> String -- ^ Block label for this session.
                      -> (ProverInterface -> IO a)
                      -> IO a
newInteractiveSession annFile cmdline allGoalCounter verifiedGoalCounter errorCounter
                      funName lbl action = do
  -- Create Goal counter for just this block.
  blockGoalCounter <- newIORef 0
  let cp = (P.shell cmdline)
           { P.std_in  = P.CreatePipe
           , P.std_out = P.CreatePipe
           , P.std_err = P.CreatePipe
           }
  createResult <- try $ P.createProcess cp
  case createResult of
    Right (Just cmdHandle, Just respHandle, Just errHandle, ph) -> do
      flip finally (P.terminateProcess ph) $ do
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
                    , proveFalseCallback = \g ->
                        interactiveVerifyGoal ictx g
                    , proveTrueCallback = \g ->
                        interactiveVerifyGoal ictx (SMT.not g)
                    , blockErrorCallback = \_ -> do
                        modifyIORef errorCounter (+1)
                    }
        r <- seq ictx $ seq fns $ action fns
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
  -- Counter for errors
  errorCounter <- newIORef 0
  let whenDone = do
        allCnt <- readIORef allGoalCounter
        verCnt <- readIORef verifiedGoalCounter
        errorCnt <- readIORef errorCounter
        if errorCnt == 0 && verCnt == allCnt then
          hPutStrLn stdout "Verification succeeded."
         else
          hPutStrLn stdout "Verification failed."
        hPutStrLn stdout $ printf "Verified %s/%s goal(s)." (show verCnt) (show allCnt)
        when (errorCnt > 0) $ do
          hPutStrLn stdout $ printf "Encountered %s error(s)." (show errorCnt)
  pure $! PSGen { blockCallbacks =
                    newInteractiveSession annFile cmdline
                      allGoalCounter verifiedGoalCounter errorCounter
                , sessionComplete = whenDone
                }

exportCheckSatProblem :: FilePath
                         -- ^ Directory to write file to.
                      -> String -- ^ Name of function
                      -> String -- ^ Name of label of block we are generating.
                      -> IORef Natural -- ^ Index of goal to discharge within block
                      -> IORef Builder.Builder  -- ^ A representation of all commands added.
                      -> SMT.Term -- ^ Negation of proposition to prove.
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
    , blockErrorCallback = \_ ->
        pure ()
    }

runVCGs :: Ann.FunctionAnn -- ^ Annotations for the function we are verifying.
        -> BlockLabel -- ^ Label of first block
        -> BlockLabel -- ^ Label of this block.
        -> Ann.ReachableBlockAnn -- ^ Annotations for the block we are verifying.
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
  liftIO $ blockCallbacks gen (Ann.llvmFunName funAnn) (ppBlock lbl) $ \prover -> do
    let blockStart = Ann.blockAddr blockAnn
    let sz = Ann.blockCodeSize blockAnn
    let blockMap = Map.fromList
          [ (segOff, Ann.eventInfo e)
          | e <- Ann.blockEvents blockAnn
            -- Get segment offset of event.
          , let ea = Ann.eventAddr e
          , let segOff =
                  if blockStart <= ea && fromIntegral (ea - blockStart) < sz then
                    case incSegmentOff thisSegOff (toInteger (ea - blockStart)) of
                      Just so -> so
                      Nothing -> error "Block extends outside of starting memory segment"
                   else
                    error $ printf "Memory event annotation address %s is out of range (low: %s, size: %d)."
                                   (show ea) (show blockStart) sz
          ]

    let ctx = BlockVCGContext { mcModuleVCGContext = modCtx
                              , curFunAnnotations = funAnn
                              , firstBlockLabel = firstLabel
                              , currentBlock = lbl
                              , callbackFns = prover
                              , mcBlockEndAddr = incAddr (toInteger sz) (segoffAddr thisSegOff)
                              , mcBlockMap = blockMap
                              }
    -- Add builtin functions
    do addCommandCallback prover $ M.evenParityDecl
       -- Add read/write operations (independent of registers)
       mapM_ (addCommandCallback prover . supportedReadDecl)  supportedMemTypes
       mapM_ (addCommandCallback prover . supportedWriteDecl) supportedMemTypes

    -- Declare registers when function starts.
    declareFunctionStartRegValues prover
    -- Declare stack and heap bounds.
    let ann = moduleAnn modCtx
    mapM_ (addCommandCallback prover) $
      mcMemDecls (Ann.pageSize ann) (Ann.stackGuardPageCount ann) (Ann.blockAllocas blockAnn)
    -- Create registers
    regs <- do
      -- Register values determined by location.
      let locRegValues = initBlockRegValues blockAnn
      -- Assert register value equals function start register value
      -- if we are in first block.
      let calleeRegValues
            | isFirstBlock =
              [ (Some r, functionStartRegValue r)
              | r <- fnStartSortedGPRegList
              ]
            | otherwise =
              []
      declareAddrStartRegValues prover (addrName (segoffAddr thisSegOff))
        (Map.fromList $ locRegValues ++ calleeRegValues)
    -- Create block state.
    let s = BlockVCGState { mcCurAddr   = thisSegOff
                          , mcCurSize   = 0
                          , mcX87Top    = fromIntegral (Ann.blockX87Top blockAnn)
                          , mcDF        = Ann.blockDFFlag blockAnn
                          , mcCurRegs  = regs
                          , mcMemIndex = 0
                          , mcEvents = []
                          , mcLocalIndex = 0
                          , mcPendingAllocaOffsetMap =
                              Map.fromList
                              [ (Ann.allocaIdent a, a)
                              | a <- Ann.blockAllocas blockAnn
                              , not (Ann.allocaExisting a)
                              ]
                          , llvmInstIndex = 0
                          , activeAllocaSet = Set.empty
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
   = VCGArgs { reoptAnnotationsPath :: !(Maybe FilePath)
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
      when (isJust (reoptAnnotationsPath args)) $ do
        throwError $ "Multiple VCG files specified."
      parseArgs rest $ args { reoptAnnotationsPath = Just path }

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

withVCGArgs :: IO (Ann.ModuleAnnotations, ProverSessionGenerator)
withVCGArgs = do
  cmdArgs <- getArgs
  let initVCG = VCGArgs { reoptAnnotationsPath = Nothing, requestedMode = DefaultMode }
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
    case reoptAnnotationsPath args of
      Nothing -> showError "Missing VCG file to run."
      Just path -> return path
  cfg <- do
    vcgResult <- Aeson.eitherDecodeFileStrict' annFile
    case vcgResult of
      Left err -> do
        hPutStrLn stderr $ "Error parsing annotations: " ++ show err
        exitFailure
      Right jsonConfig -> do
        let llvmMap = undefined
        case Aeson.iparse (Ann.parseAnnotations llvmMap) jsonConfig of
          Aeson.IError path msg -> do
            hPutStrLn stderr $ "Error parsing annotations: " ++ Aeson.formatError path msg
            exitFailure
          Aeson.ISuccess cfg -> do
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
      psGen <- interactiveSMTGenerator annFile "cvc4 --lang=smt2 --dedicated-eqrange-quant --incremental"
      pure (cfg, psGen)
    RunSolver cmdline -> do
      psGen <- interactiveSMTGenerator annFile cmdline
      pure (cfg, psGen)

standaloneGoalFilename :: String -- ^ Name of function to verify
                       -> String  -- ^ Pretty printed version of block label.
                       -> Natural -- ^ Index of goal to discharge within block
                       -> FilePath
standaloneGoalFilename fn lbl i = fn ++ "_" ++ lbl ++ "_" ++ show i ++ ".smt2"


-- | Define LLVM arguments in terms of the function start value of
-- machine code registers.
defineLLVMArgs :: [Typed Ident]
               -> [X86Reg (M.BVType 64)] -- ^ Remaining registers for arguments.
               -> BlockVCG ()
defineLLVMArgs [] _x86Regs = pure ()
defineLLVMArgs (Typed (PrimType (Integer 64)) val : rest) x86Regs =
  case x86Regs of
    [] -> error $ "Ran out of register arguments."
    (reg:restRegs) -> do
      addCommand $ SMT.defineFun (identVar val) [] (SMT.bvSort 64)
                                 (functionStartRegValue reg)
      defineLLVMArgs rest restRegs
defineLLVMArgs (Typed tp _val : _rest) _x86Regs =
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
          printf "  Allocation %s overlaps with %s" (show (Ann.allocaIdent x)) (show (Ann.allocaIdent y))

-- | Loop over LLVM stmts
--
-- Note. This is written to take a function rather than directly call
-- @stepNextStmtg@ so that the call stack is cleaner.
loopStmts :: (L.Stmt -> BlockVCG Bool) -> [L.Stmt] -> BlockVCG ()
loopStmts _ [] = do
  error $ "We have reached end of LLVM events without a block terminator."
loopStmts f (stmt:stmts) = do
  c <- f stmt
  modify' $ \s -> s { llvmInstIndex = llvmInstIndex s + 1 }
  if c then
    loopStmts f stmts
   else
    unless (null stmts) $ error "Expected return to be last LLVM statement."


-- | Verify a block satisfies its specification.
verifyBlock :: HasCallStack
            => Define
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
  mblockAnn <-
    case findBlock funAnn lbl of
      Just b -> pure b
      Nothing ->
        moduleThrow $
          printf "%s: Block %s lacks annotations." (Ann.llvmFunName funAnn) (ppBlock lbl)
  case mblockAnn of
    -- We only need to verify unreachable blocks are not reachable."
    Ann.UnreachableBlock -> do
      pure ()
    Ann.ReachableBlock blockAnn -> do
      -- Check allocations do not overlap with each other.
      checkAllocaOverlap lbl (Ann.blockAllocas blockAnn)
      -- Start running verification condition generator.
      runVCGs funAnn firstLabel lbl blockAnn $ do
        -- Add LLVM declarations for all existing allocations.
        forM_ (Ann.blockAllocas blockAnn) $ \a  -> do
          when (Ann.allocaExisting a) $ do
            allocaDeclarations (Ann.allocaIdent a) (Ann.allocaSize a)
        -- Declare memory
        addCommand $ SMT.declareConst (memVar 0) memSort
        -- Declare constant representing where we return to.
        defineVarFromReadMCMem "return_addr" stackHighTerm addrSupportedMemType
        -- Declare LLVM arguments in terms of Macaw registers at function start.
        defineLLVMArgs (L.defArgs lFun) x86ArgGPRegs
        -- Assume preconditions
        do regs <- gets mcCurRegs
           mem  <- getMCMem
           forM_ (Ann.blockPreconditions blockAnn) $ \p -> do
             addAssert (evalPrecondition regs mem p)
        -- Start processing LLVM statements
        loopStmts stepNextStmt (bbStmts bb)

$(pure [])


-- | Verify a particular function satisfies its specification.
verifyFunction :: HasCallStack
               => Module
               -- ^ LLVM Module
               -> Ann.FunctionAnn
               -- ^ Annotations to add in mapping LLVM module and
               -- memory layout.
               -> ModuleVCG ()
verifyFunction lMod funAnn = do
  modCtx <- ask
  let fnm :: String
      fnm = Ann.llvmFunName funAnn
  vcgLog $ "Analyzing " ++ fnm

  lFun <-
    case getDefineByName lMod fnm of
      Just f -> pure f
      Nothing -> moduleThrow $ printf "Could not find LLVM function %s in module." fnm

  when (length (L.defArgs lFun) > length Ann.x86ArgGPRegs) $ do
    moduleThrow $ "Too many arguments."

  case defBody lFun of
    [] -> moduleThrow $ "Expected function to have at least one basic block."
    firstBlock:restBlocks -> do
      let Just entryLabel = bbLabel firstBlock
      firstBlockAnn <-
        case findBlock funAnn entryLabel of
          Just (Ann.ReachableBlock b) -> pure b
          Just Ann.UnreachableBlock -> do
            moduleThrow $
              printf "%s: Entry block %s must be reachable." fnm (ppBlock entryLabel)
          Nothing ->
            moduleThrow $
              printf "%s: Could not find annotations for LLVM block %s." fnm (ppBlock entryLabel)
      let Right addr = getMCAddrOfLLVMFunction (symbolAddrMap modCtx) fnm
      when (toInteger addr /= toInteger (Ann.blockAddr firstBlockAnn)) $ do
        moduleThrow $ printf "%s annotations list address of %s; symbol table reports address of %s."
                             fnm (show (Ann.blockAddr firstBlockAnn)) (show addr)
      -- Verify the blocks.
      forM_ (firstBlock:restBlocks) $ \bb -> do
        moduleCatch $ verifyBlock lFun funAnn entryLabel bb

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

-- | Wait for a process to end and throw an error case if the return
-- value differs from the 'ExitSuccess'.
--
-- This function takes a handle to the process's stderr and closes it
-- or semicloses it before returning.
waitForEnd :: String -- ^ Name of tool
           -> Handle -- ^ Handle to read from for getting error
           -> P.ProcessHandle -- ^ Handle to process
           -> IO ()
waitForEnd tool errHandle ph = do
  ec  <- P.waitForProcess ph
  case ec of
    ExitSuccess ->
      hClose errHandle
    ExitFailure c -> do
      msg <- hGetContents errHandle
      let msg' | null msg = tool ++ " exited with error code " ++ show c ++ "."
               | otherwise = tool ++ ": " ++ msg
      hPutStrLn stderr msg'

-- | Try to create a process, and call the call back funtion with
-- the handles for 'stdin', 'stdout', and 'stderr' if it succeeds.
--
-- This will throw a failure and read from standard error is the process fails.
--
-- If the attempt fails, because the process does not exist, then throw the given
-- exception.
withCreateProcess :: String -- ^ Name of tool for error purposes
                  -> String -- ^ Path of program to execute
                  -> [String] -- ^ Arguments
                  -> ((Handle, Handle, Handle) -> IO a)
                  -> IO a
withCreateProcess nm cmd args f = do
  let cp = (P.proc cmd args)
                { P.env = Nothing
                , P.std_in  = P.CreatePipe
                , P.std_out = P.CreatePipe
                , P.std_err = P.CreatePipe
                }
  let h :: IOException -> IO a
      h ioe
        | isDoesNotExistError ioe = do
            hPutStrLn stderr "Could not find llvm-as"
            exitFailure
        | isPermissionError ioe = do
            hPutStrLn stderr "Do not have permission to execute llvm-as"
            exitFailure
        | otherwise = throwIO ioe
  (Just in_handle, Just out, Just err, ph) <-
     P.createProcess cp `catch` h
  f (in_handle, out, err) <* waitForEnd nm err ph

-- | This is a helper function that runs an IO action and ensures that
-- some cleanup is performed if an exception is thrown.
writeAndClose :: Handle -- ^ Input handle to write to.
              -> Handle -- ^ Error handle to close if things fail.
              -> IO () -- ^ Action to run
              -> IO ()
writeAndClose inHandle errHandle action = do
  let h :: IOError -> IO ()
      h e | ioeGetErrorType e == ResourceVanished = do
              hClose inHandle
              hPutStrLn stderr =<< hGetContents errHandle
              exitFailure
          | otherwise = do
              hClose inHandle
              hClose errHandle
              throwIO (e :: IOError)
  action `catch` h
  hClose inHandle


-- | Run llvm-as to generate an bitcode file from text.
runLlvmAs :: FilePath
          -- ^ Path to llvm-as
          -> BS.ByteString
          -- ^ .ll file file
          -> IO BS.ByteString
runLlvmAs cmd asm = do
  -- Run GNU asssembler
  let args= []
  withCreateProcess "llvm-as" cmd args $ \(inHandle, outHandle, errHandle) -> do
    -- Write to input handle and close it.
    writeAndClose inHandle errHandle $ do
      hSetBinaryMode inHandle True
      BS.hPut inHandle asm
    -- Get output
    hSetBinaryMode outHandle True
    BS.hGetContents outHandle

-- | Parse thte Get LLVM module
getLLVMBCModule :: BS.ByteString -> IO Module
getLLVMBCModule bs = do
  res <- parseBitCode bs
  case res of
    Left err -> do
      hPutStrLn stderr $ "Could not parse LLVM: " ++ show err
      exitFailure
    Right m ->
      pure m

-- | Parse the LLVM  module either in .bc or .ll format.
getLLVMModule :: FilePath -> IO Module
getLLVMModule path = do
  bs <- BS.readFile path
  if BS.take 4 bs == "BC\xc0\xde" then
    getLLVMBCModule bs
   else if takeExtension path == ".ll" then do
    hPutStrLn stderr "Reading assembly"
    bcBS <- runLlvmAs "llvm-as" bs
    getLLVMBCModule bcBS
   else do
    hPutStrLn stderr $ "Could not determine type of LLVM file: " ++ path ++ "\n"
      ++ show (BS.take 4 bs)
    exitFailure

main :: IO ()
main = do
  (ann, gen) <- withVCGArgs
  e <- readElf $ Ann.binFilePath ann
  let loadOpts = defaultLoadOptions
  (warnings, mem, _entry, symbols) <-
    case resolveElfContents loadOpts e of
      Left err -> do
        hPutStrLn stderr $
          "Could not interpret Elf file " ++ Ann.binFilePath ann ++ ":\n"
          ++ "  " ++ err
        exitFailure
      Right r -> pure r

  forM_ warnings $ \w -> do
    hPutStrLn stderr w

  -- Get LLVM module
  lMod <- getLLVMModule (Ann.llvmFilePath ann)
  -- Create verification coontext for module.
  errorRef <- newIORef 0
  let modCtx = ModuleVCGContext { moduleAnn = ann
                                , moduleMem = mem
                                , symbolAddrMap = Map.fromList
                                                  [ (memSymbolName sym, memSymbolStart sym)
                                                  | sym <- symbols
                                                  ]
                                , writeStderr = True
                                , errorCount = errorRef
                                , proverGen = gen
                                , moduleTypeMap = Map.fromList
                                    [ (nm,tp)
                                    | L.TypeDecl nm tp <- L.modTypes lMod
                                    ]

                                }
  -- Run verification.
  runModuleVCG modCtx $ do
    forM_ (Ann.functions ann) $ \funAnn -> do
      moduleCatch $ verifyFunction lMod funAnn
  errorCnt <- readIORef errorRef
  if errorCnt > 0 then
    hPutStrLn stderr "Errors during verification."
   else
    sessionComplete gen
