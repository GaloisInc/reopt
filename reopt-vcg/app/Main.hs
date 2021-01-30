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
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Internal as Aeson
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ElfEdit as Elf
import           Data.Foldable
import qualified Data.HashMap.Strict as HMap
import           Data.IORef
import           Data.Int
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
import qualified Data.Vector as V
import           GHC.IO.Exception
import           GHC.Stack
import           Numeric
import           Numeric.Natural
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

import           LLVMLoader
import           VCGCommon
import qualified VCGMacaw as M

$(pure [])

-- | Pretty print a block label for display purposes.
ppBlock :: BlockLabel -> String
ppBlock (Named (Ident s)) = s
ppBlock (Anon i) = show i

$(pure [])

-- | Maps phi variable names to their corresponding type and values
-- for each source block.
type PhiVarMap = HMap.HashMap Text (Type, Map BlockLabel L.Value)

-- | Information about a block including annotations
data AnnotatedBlock
  = AnnotatedBlock { abAnn :: !Ann.BlockAnn
                   , abLbl :: !BlockLabel -- ^ Label for block
                   , abPhiVarMap :: !PhiVarMap
                   , abStmts :: ![L.Stmt]
                     -- ^ Statements after phi variables.
                   }


-- | Maps LLM block labels to their associated annotations.
type ReachableBlockAnnMap = HMap.HashMap String AnnotatedBlock

-- | Find a block with the given label in the config.
findBlock :: ReachableBlockAnnMap -> BlockLabel -> Maybe (Ann.BlockAnn, PhiVarMap)
findBlock m lbl = do
  ab <- HMap.lookup (ppBlock lbl) m
  pure (abAnn ab, abPhiVarMap ab)

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
  , blockErrorCallback :: !(Int -> MemSegmentOff 64 -> String -> IO ())
    -- ^ Report a block error given LLVM instruction index and machine code address.
  }

$(pure [])

type FunctionName = String
-- ^ The name of a function in the annotation.
--
-- This is the name in LLVM.

-- | Function for creating sessions to interact with SMT solver.
--
-- As blocks can be independently verified, we create a separate
-- prover interface for each block to be verified.
data ProverSessionGenerator
   = PSGen { blockCallbacks :: FunctionName -> BlockLabel -> (ProverInterface -> IO ()) -> IO ()
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

-- | Errors that are tied to a specific function.
data FunctionError
   = FunctionNotFound
   | FunctionArgTypeUnsupported !Ident L.Type
   | FunctionMissingEntryBlock
   | FunctionEntryUnreachable
   | SomeFunctionError !String
   deriving (Show)

instance IsString FunctionError where
  fromString = SomeFunctionError

ppFunctionError :: FunctionError -> String
ppFunctionError FunctionNotFound = "Could not find definition in LLVM."
ppFunctionError (FunctionArgTypeUnsupported (Ident nm) tp) =
  printf "Function argument %s has unsupported type %s." nm (show (L.ppType tp))
ppFunctionError FunctionMissingEntryBlock =
  "Function body is missing an entry block."
ppFunctionError FunctionEntryUnreachable =
  "Function entry marked unreachable."
ppFunctionError (SomeFunctionError msg) = msg

-- | Errors that are tied to a specific
data BlockError
   = BlockAnnParseFailure !String
   | BlockMissingAnnotations
   | BlockUnsupportedPhiVarType !Ident !Type
   | BlockAddrInvalid !(MemWord 64)
   deriving (Show)

ppBlockError :: BlockError -> String
ppBlockError (BlockAnnParseFailure msg) = printf "Annotation parse failure: %s" msg
ppBlockError BlockMissingAnnotations = "Could not find block annotations."
ppBlockError (BlockUnsupportedPhiVarType (Ident nm) tp) =
  printf "Phi variable %s has unsupported type %s." nm (show (L.ppType tp))
ppBlockError (BlockAddrInvalid addr) =
  printf "Annotated block address %s is not not in code segment." (show addr)

data ModuleError
   = ModuleError String
   | FunctionError !FunctionName !FunctionError
   | BlockError !FunctionName !BlockLabel !BlockError
     -- ^ @BlockAnnParseFailure fn Could not parse JSON object
  deriving (Typeable, Show)

renderBlockError :: FunctionName -> BlockLabel -> String -> String
renderBlockError fnm lbl msg = printf "%s.%s. %s" fnm (ppBlock lbl) msg

-- | Pretty print an error that occurs at the start of an instruction.
renderMCInstError :: FunctionName -- ^ Name of function
                  -> BlockLabel -- ^ Block label
                  -> Int -- ^ LLVM instruction index
                  -> MemSegmentOff 64 -- ^ Address of current instruction.
                  -> String
                  -> String
renderMCInstError fnm lbl idx addr msg =
  printf "%s.%s.%d (%s). %s" fnm (ppBlock lbl) idx (showsPrec 10 addr "") msg

instance Exception ModuleError where
  displayException e =
    case e of
      ModuleError msg -> msg
      FunctionError fnm funErr ->
        printf "%s. %s" fnm (ppFunctionError funErr)
      BlockError fnm lbl blockErr ->
        renderBlockError fnm lbl (ppBlockError blockErr)

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

-- | A warning that stops execution until catch.
functionError :: FunctionName -> FunctionError -> ModuleVCG a
functionError fnm e = liftIO (throwIO (FunctionError fnm e))

-- | A warning that stops execution until catch.
blockError :: FunctionName -> BlockLabel -> BlockError -> ModuleVCG a
blockError fnm lbl e = liftIO (throwIO (BlockError fnm lbl e))

-- | A warning that stops execution until ca1tch.
moduleThrow :: String -> ModuleVCG a
moduleThrow = liftIO . throw . ModuleError

-- | Catch a VCG error, print it to the screen and keep going.
moduleCatch :: ModuleVCG () -> ModuleVCG ()
moduleCatch (ModuleVCG m) = ModuleVCG $ ReaderT $ \ctx -> do
  catch (runReaderT m ctx) $ \(e :: ModuleError) -> do
    when (writeStderr ctx) $ do
      hPutStrLn stderr $ "Error: " ++ displayException e
    modifyIORef' (errorCount ctx) (+1)

$(pure [])

------------------------------------------------------------------------
-- BlockVCG

-- | Information that does not change during execution of @BlockVCG@.
data BlockVCGContext = BlockVCGContext
  { mcModuleVCGContext :: !ModuleVCGContext
    -- ^ Information at module level about CFG.
  , llvmFunName :: !String
    -- ^ Annotations for the current function.
  , funBlkAnnotations :: !ReachableBlockAnnMap
    -- ^ Annotations for blocks in the CFG.
  , firstBlockLabel :: !BlockLabel
    -- ^ Label for first block in this function
  , currentBlock :: !BlockLabel
    -- ^ Label for block we are verifying.
  , callbackFns :: !ProverInterface
    -- ^ Functions for interacting with SMT solver.
  , mcBlockEndAddr :: !(MemAddr 64)
    -- ^ The end address of the block.
  , mcBlockMap :: !(Map (MemSegmentOff 64) Ann.MemoryAnn)
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
  , mcPendingAllocaOffsetMap :: !(Map Ann.LocalIdent Ann.AllocaAnn)
    -- ^ This is a map from allocation names to the annotations about their
    -- size and offset.
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
  thisFun <- asks $ llvmFunName
  thisBlk <- asks $ currentBlock
  thisInst <- gets $ llvmInstIndex
  addr <- gets $ mcCurAddr
  return $! renderMCInstError thisFun thisBlk thisInst addr msg

$(pure [])

-- | Report an error at the given location and stop verification of
-- this block.
fatalBlockError :: String -> BlockVCG a
fatalBlockError msg = do
  thisInst <- gets $ llvmInstIndex
  addr <- gets $ mcCurAddr
  callback <- asks $ blockErrorCallback . callbackFns
  liftIO $ callback thisInst addr msg
  haltBlock

$(pure [])

addCommand :: SMT.Command -> BlockVCG ()
addCommand cmd = do
  prover <- asks callbackFns
  liftIO $ addCommandCallback prover cmd

$(pure [])

-- | Add assertion that the propositon is true without requiring it to be proven.
addAssert :: SMT.Term -> BlockVCG ()
addAssert p = addCommand $ SMT.assert p

-- | @proveTrue p msg@ adds a proof obligation @p@ is true for all
-- interpretations of constants with the message @msg@.
proveTrue :: HasCallStack => SMT.Term -> String -> BlockVCG ()
proveTrue p msg = do
  annMsg <- prependLocation msg
  fns <- asks callbackFns
  liftIO $ proveTrueCallback fns p annMsg
  addAssert p

-- | @proveEq x y msg@ add a proof obligation named @msg@ asserting
-- that @x@ equals @y@.
proveEq :: HasCallStack => SMT.Term -> SMT.Term -> String -> BlockVCG ()
proveEq x y msg = do
  fns <- asks callbackFns
  annMsg <- prependLocation msg
  liftIO $ proveFalseCallback fns (SMT.distinct [x,y]) annMsg
  -- Add command for future proofs
  addAssert (SMT.eq [x,y])

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
  let expectedAddrTerm = bvhexadecimal (toInteger addr) 64
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
          let ptr = SMT.bvadd ptr0 [bvdecimal (toInteger i) 64]
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
addc w t i = SMT.bvadd t [bvdecimal (toInteger i) w]

-- | @subc w x y@ returns an expression equal to @bvsub x y@.
--
-- @w@ should be positive.
subc :: Natural -> SMT.Term -> Natural -> SMT.Term
subc _ t 0 = t
subc w t i = SMT.bvsub t (bvdecimal (toInteger i) w)

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
  SMT.term_app (Builder.fromText nm) [a, bvdecimal (toInteger sz) 64]

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
  SMT.term_app "not_in_stack_range" [addr, bvdecimal (toInteger sz) 64]

-- | @stackHighTerm@ denotes the top of the stack.
stackHighTerm :: SMT.Term
stackHighTerm = functionStartRegValue RSP

-- | @allocaMCBaseEndDecls a@ introduces variables for defining the
-- extent in the machine code stack of an LLVM alloca.
allocaMCBaseEndDecls :: Ann.AllocaAnn -> [SMT.Command]
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
    SMT.eq [ SMT.bvand a [bvhexadecimal (toInteger (sz-1)) 64]
           , bvdecimal 0 64
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
           -> [Ann.AllocaAnn]
              -- ^ Allocations
           -> [SMT.Command]
mcMemDecls pageSize guardPageCount allocas
  | guardPageCount == 0 = error "guardPageCount must be positive."
  | otherwise =
  (let guardSize = pageSize * guardPageCount
    in [ SMT.declareConst "stack_alloc_min" (SMT.bvSort 64)
       , SMT.assert $ isPageAligned "stack_alloc_min" pageSize
       , SMT.assert $ SMT.bvult (bvdecimal (toInteger guardSize) 64) "stack_alloc_min"

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
       , SMT.assert $ SMT.eq [ SMT.extract 3 0 stackHighTerm, bvdecimal 8 4]
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
      -- A MCOnlyStack read means the machine code reads memory, but
      -- LLVM does not.
      --
      -- We currently check that these reads only access the stack as
      -- the only current use of these annotations is to mark register
      -- spills, return address read/writes, and frame pointer/callee saved
      -- register saves/restores.
      --
      -- Checking this is on the stack also ensures there are no side effects
      -- from mem-mapped IO reads since the stack should not be mem-mapped IO.
      proveTrue (evalRangeCheck onStack mcAddr (memReprBytes tp)) $
           "Machine code read is in unallocated stack space."
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
      -- If we run out of events for this instruction, then get events
      -- for next instruction.
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
primEval :: Type -> L.Value -> BlockVCG SMT.Term
primEval _ (ValIdent var) = do
  pure $! varTerm (identVar var)
primEval (PrimType (Integer w)) (ValInteger i) = do
  when (w <= 0) $ error "primEval given negative width."
  pure $! bvdecimal i (fromIntegral w)
primEval (PrimType (Integer 1)) (ValBool b) = do
  pure $! bvdecimal (if b then 1 else 0) 1
primEval tp v  =
  error $ "Error: Missing case in primEval:\n"
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
  -- Check we are calling the same function in LLVM and binary.
  lArgs <- traverse evalTyped args
  let mRegIP = getConst $ regs ^. boundValue X86_IP
  assertFnNameEq fsym mRegIP
  missingFeature "Check varargs functions pass in correct number of arguments."
  -- Verify that the arguments should be same.  Note: Here we take the
  -- number of arguments from LLVM side, since the number of arguments
  -- in Macaw side seems not explicit.  Also assuming that the # of
  -- arguments of LLVM side is less or equal than six.
  when (length lArgs > length x86ArgGPRegs) $ do
    error $ "Too many arguments."
  -- Check LLVM and machine code arguments match
  do let compareArg :: (Int, SMT.Term, X86Reg (M.BVType 64)) -> BlockVCG ()
         compareArg (idx, la, reg) = do
           let Const ma = regs^.boundValue reg
           proveEq la ma $ printf "LLVM argument %d is equal to value in register %s." idx (show reg)
     traverse_ compareArg (zip3 [0..] lArgs x86ArgGPRegs)

  -- Check direction flag is clear
  checkDirectionFlagClear

  -- Get address of next instruction as an SMT term.
  nextInsnAddr <- gets $ mcNextAddr

  -- Get value of RSP at the function call.
  let curRSP :: SMT.Term
      curRSP = getConst (regs^.boundValue RSP)

  -- Check we saved return address on call.
  let postCallRIP :: SMT.Term
      postCallRIP = M.evalMemAddr nextInsnAddr
  do -- Get value stored at return address
     mem <- gets  $ varTerm . memVar . mcMemIndex
     -- Check stored return value matches next instruction
     proveEq (readFromMemory mem curRSP addrSupportedMemType) postCallRIP
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
          ++ [ (Some X86_IP, postCallRIP)
             , (Some RSP,    postCallRSP)
             , (Some DF,     SMT.false)
             , (Some X87_TopReg, bvdecimal 7 3)
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
                   -> SMT.Term -- ^ Number of bytes
                   -> BlockVCG ()
allocaDeclarations nm sz = do
  -- Get used allocas
  used <- gets activeAllocaSet
  -- Check that alloca name is not in use.
  when (Set.member nm used) $ error $ show nm ++ " is already used an allocation."
  -- Add alloca name to active set.
  modify' $ \s -> s { activeAllocaSet = Set.insert nm used }
  -- Declare LLVM alloca base and end
  addCommand $ SMT.declareConst (allocaLLVMBaseVar nm) ptrSort
  addCommand $ SMT.defineFun (allocaLLVMEndVar nm) [] ptrSort $
    SMT.bvadd (allocaLLVMBaseVar nm) [sz]
  -- Assert alloca end computation did not overflow.
  addAssert $ SMT.bvule (allocaLLVMBaseVar nm) (allocaLLVMEndVar nm)
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
        fatalBlockError $ printf "Missing annotation on alloca %s." (show nm)
      Just a -> pure a

  when (Ann.allocaExisting a) $ do
    fatalBlockError $
      printf "Allocation %s annotation indicates it should have already been created." (show nm)
  -- Mark this as existing.
  modify $ \s -> s { mcPendingAllocaOffsetMap = Map.insert nm a { Ann.allocaExisting = True } allocaMap }

  -- Check the size of LLVM allocation matches annotations.
  case eltCount of
    Nothing -> do
      when (eltSize /= Ann.allocaSize a) $
        fatalBlockError $
           printf "Allocation size at %s must match specification %s."
             nm0 (show (Ann.allocaSize a))
    Just (Typed (PrimType (Integer w)) i) | w >= 1 -> do
       let (q,r) = Ann.allocaSize a `quotRem` eltSize
       when (r /= 0) $ do
         fatalBlockError $ "Expect alloca size to be a multple of element size."
       when (q >= 2^w) $ do
         fatalBlockError $ "Specified allocation is outside range of alloca size."
       cntExpr <- primEval (PrimType (Integer w)) i
       let wn = fromIntegral w
       proveEq cntExpr (bvdecimal (toInteger q) wn) $
         printf "Allocation size at %s must match specification %s."
                nm0 (show (Ann.allocaSize a))
    Just (Typed itp _) -> do
      fatalBlockError $ "Unexpected allocation count type " ++ show (L.ppType itp)

  -- Create declarations for alloca.
  allocaDeclarations nm (bvdecimal (toInteger (Ann.allocaSize a)) 64)

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
llvmLoad ident src llvmType malign = do
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
  -- Assert the stack height at the return is just above the return
  -- address pointer.
  proveEq (getConst (regs^.boundValue RSP)) (addc 64 stackHighTerm 8)
    "Stack height at return matches init."

  -- Assert the IP after the fetch and execute is the return address
  proveEq (getConst (regs^.boundValue X86_IP)) (varTerm "return_addr")
    "Return address matches entry value."

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

-- | Tell the SMT solver that the given LLVM identifier has the given
-- SMT sort and definition.
defineIdent :: Ident -> SMT.Sort -> SMT.Term -> BlockVCG ()
defineIdent nm tp t = do
  addCommand $ SMT.defineFun (identVar nm) [] tp t

$(pure [])

llvmError :: String -> a
llvmError msg = error ("[LLVM Error] " ++ msg)

arithOpFunc :: ArithOp -> SMT.Term -> SMT.Term -> BlockVCG SMT.Term
arithOpFunc (Add _uw _sw) x y = pure $! SMT.bvadd x [y]
arithOpFunc (Sub _uw _sw) x y = pure $! SMT.bvsub x y
arithOpFunc (Mul _uw _sw) x y = pure $! SMT.bvmul x [y]
arithOpFunc _ _ _ = llvmError "Not implemented yet"

bitOpFunc :: BitOp -> SMT.Term -> SMT.Term -> BlockVCG SMT.Term
bitOpFunc And x y = pure $! SMT.bvand x [y]
bitOpFunc Or  x y = pure $! SMT.bvor  x [y]
bitOpFunc Xor x y = pure $! SMT.bvxor x [y]
bitOpFunc _ _ _ = llvmError "Not implemented yet"

-- | Sort for LLVM iX with the given width.
llvmISort :: Int32 -> SMT.Sort
llvmISort i = SMT.bvSort (fromIntegral i)

-- | Convert LLVM type to SMT sort.
asSMTSort :: Type -> Maybe SMT.Sort
asSMTSort (PtrTo _) = Just (SMT.bvSort 64)
asSMTSort (PrimType (Integer i)) | i > 0 = Just $ llvmISort i
asSMTSort _ = Nothing

-- | Return the definition in the module with the given name.
getDefineByName :: Module -> String -> Maybe Define
getDefineByName llvmMod name =
  List.find (\d -> defName d == Symbol name) (modDefines llvmMod)

$(pure [])

-- | Register values initialized from annotations.
initBlockRegValues :: Ann.ReachableBlockAnn -> [(Some X86Reg, SMT.Term)]
initBlockRegValues blockAnn =
  [ (Some X86_IP,     bvhexadecimal (toInteger (Ann.blockAddr blockAnn)) 64)
  , (Some X87_TopReg, bvdecimal (toInteger (Ann.blockX87Top blockAnn)) 3)
  , (Some DF,         if Ann.blockDFFlag blockAnn then SMT.true else SMT.false)
  ]

$(pure [])

-- | Evaluate a annoatation expression to construct an SMT term.
evalPrecondition :: (Text -> SMT.Term)
                    -- ^ Function for mapping LLVM phi variables to
                    -- their definition.
                 -> RegState X86Reg (Const SMT.Term)
                    -- ^ Map from registers to LLVM term.
                 -> SMT.Term
                    -- ^ Expression representing memory.
                 -> Ann.Expr Ann.BlockVar
                    -- ^ Expression with precondition.
                 -> SMT.Term
evalPrecondition phiTermFn regs mem e = do
  let r = evalPrecondition phiTermFn regs mem
  case e of
    Ann.Eq x y -> SMT.eq [r x, r y]
    Ann.BVAdd x y -> SMT.bvadd (r x) [r y]
    Ann.BVSub x y -> SMT.bvsub (r x) (r y)
    Ann.BVDecimal x y -> bvdecimal (toInteger x) y
    Ann.Var v ->
      case v of
        Ann.StackHigh -> stackHighTerm
        Ann.InitGPReg64 reg -> getConst $ regs^.boundValue (X86_GP reg)
        Ann.FnStartGPReg64 reg ->
          functionStartRegValue (X86_GP reg)
        Ann.MCStack addr bitCount ->
          readFromMemory mem (r addr) (supportedBVMemType (bitCount `shiftR` 3))
        Ann.LLVMVar nm ->
          phiTermFn nm

$(pure [])

-- | @verifyBlockPreconditions f lbl@ verifies the preconditions for block @lbl@
-- are satisfied in the current state.
--
-- The function @f@ is applied to each predicate before verification,
-- and allows us to conditionally validate some of the preconditions.
verifyBlockPreconditions :: String
                         -> (SMT.Term -> SMT.Term)
                         -> BlockLabel -- ^ LLVM Label of block we are jumping to.
                         -> BlockVCG ()
verifyBlockPreconditions prefix f lbl = do
  blkMap <- asks $ funBlkAnnotations
  case findBlock blkMap lbl of
    Nothing -> do
      fatalBlockError $
        printf "Target block %s lacks annotations." (ppBlock lbl)
    Just (Ann.UnreachableBlock,_) ->
      proveTrue (f SMT.false) $
        printf "Target block %s is unreachable." (ppBlock lbl)
    Just (Ann.ReachableBlock tgtBlockAnn, varMap) -> do
      firstLabel <- asks firstBlockLabel

      when (lbl == firstLabel) $ error "LLVM should not jump to first label in function."

      -- Get current registers
      regs <- gets mcCurRegs
      -- Check initialized register values
      forM_ (initBlockRegValues tgtBlockAnn) $ \(Some r, expected) -> do
        -- Get register values
        let mcValue = getConst $ regs ^. boundValue r
        proveTrue (f (SMT.eq [expected, mcValue])) $ printf "Checking %s register %s." prefix (show r)

      mem  <- getMCMem

      srcLbl <- asks currentBlock

      -- Resolve terms for SMT variables.
      let resolvePhiVarValue :: Text -> (Type, Map BlockLabel L.Value) -> BlockVCG SMT.Term
          resolvePhiVarValue nm (tp, valMap) = do
            case Map.lookup srcLbl valMap of
              Just v -> primEval tp v
              Nothing -> error $ printf "Could not find initial value of %s." (Text.unpack nm)
      phiTermMap <- HMap.traverseWithKey resolvePhiVarValue varMap

      let resolveVar :: Text -> SMT.Term
          resolveVar phiVar =
            case HMap.lookup phiVar phiTermMap of
              Nothing -> error $ "Unassigned variable " ++ Text.unpack phiVar
              Just t -> t

      -- Check preconditions
      forM_ (Ann.blockPreconditions tgtBlockAnn) $ \p -> do
        proveTrue (f (evalPrecondition resolveVar regs mem p)) $
          printf "%s precondition: %s" prefix (Ann.exprToText p)

      -- Check allocations are preserved.
      curAllocas <- gets mcPendingAllocaOffsetMap
      when (Ann.blockAllocas tgtBlockAnn /= curAllocas) $ do
        fatalBlockError $ printf "Allocations in jump to %s do not match." (ppBlock lbl)

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
stepNextStmt (L.Result ident inst _mds) = do
  case inst of
    Phi _tp _values -> do
      error $ "stepNextStmt expected phi statements to be stripped."
    Alloca ty eltCount malign -> do
      llvmAlloca ident ty eltCount malign
      pure True
    Arith lop (Typed lty x) y -> do
      s <- coerceToSMTSort lty
      xv   <- primEval lty x
      yv   <- primEval lty y
      defineIdent ident s =<< arithOpFunc lop xv yv
      pure True
    Bit bop (Typed tp@(PrimType (Integer i)) x) y -> do
      xv <- primEval tp x
      yv <- primEval tp y
      let s = llvmISort (fromIntegral i)
      defineIdent ident s =<< bitOpFunc bop xv yv
      pure True
    Call isTailCall retty f args -> do
      -- Evaluate function
      case f of
        ValSymbol fSym ->
          llvmInvoke isTailCall fSym args (Just (ident, retty))
        _ -> missingFeature $ "VCG currently only supports direct calls."
      -- Add invoke event
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
      defineIdent ident smtResultType result
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
      defineIdent ident (SMT.bvSort 1) (SMT.ite r (bvdecimal 1 1) (bvdecimal 0 1))
      pure True
    Load (Typed (PtrTo lty) src) ord malign -> do
      when (isJust ord) $ do
        error $ "Do not yet support atomic ordering."
      llvmLoad ident src lty malign
      pure True
    _ -> do
      error $ "stepNextStmt: unsupported instruction: " ++ show inst
stepNextStmt (L.Effect instr _mds) = do
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
      let c = SMT.eq [cndTerm, bvdecimal 1 1]
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

------------------------------------------------------------------------
-- Interactive session

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

-- | Run a solver command and catch any errors from solver crashing.
catchSolverWriteFail :: InteractiveContext -> IO a -> IO a
catchSolverWriteFail ictx m = m `catch` h
  where exitOnEOF :: IOError -> IO a
        exitOnEOF e | isEOFError e = exitFailure
                    | otherwise = do
                      hPutStrLn stderr "exitOnEOF throw"
                      throwIO e
        h :: IOError -> IO a
        h e =
          case ioeGetErrorType e of
            ResourceVanished -> do
              hPutStrLn stderr $ "Error reported from solver:"
              let loop :: IO a
                  loop = do
                    msg <- hGetLine (ictxErrHandle ictx) `catch` exitOnEOF
                    hPutStrLn stderr $ "  " ++ msg
                    loop
              loop
            _tp -> do
              hPutStrLn stderr $ "Connection to solver failed: " ++ show e
              exitFailure

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
  catchSolverWriteFail ictx $ do
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
                      -> FilePath -- ^ Path to SMT solver
                      -> [String] -- ^ Arguments
                      -> IORef Natural -- ^ Counter for each goal
                      -> IORef Natural -- ^ Counter for each verified goal.
                      -> IORef Natural -- ^ Counter for errors
                      -> FunctionName -- ^ Name of function
                      -> BlockLabel -- ^ Block label for this session.
                      -> (ProverInterface -> IO ())
                      -> IO ()
newInteractiveSession annFile solver solverArgs allGoalCounter verifiedGoalCounter errorCounter
                      funName lbl action = do
  -- Create Goal counter for just this block.
  blockGoalCounter <- newIORef 0
  let cp = (P.proc solver solverArgs)
           { P.std_in  = P.CreatePipe
           , P.std_out = P.CreatePipe
           , P.std_err = P.CreatePipe
           }
  createResult <- try $ P.createProcess cp
  case createResult of
    Right (Just cmdHandle, Just respHandle, Just errHandle, ph) -> do
      let ictx = InteractiveContext { ictxAnnFile = annFile
                                    , ictxFunName = funName
                                    , ictxBlockLabel = ppBlock lbl
                                    , ictxAllGoalCounter = allGoalCounter
                                    , ictxVerifiedGoalCounter = verifiedGoalCounter
                                    , ictxBlockGoalCounter = blockGoalCounter
                                    , ictxCmdHandle = cmdHandle
                                    , ictxRespHandle = respHandle
                                    , ictxErrHandle = errHandle
                                    }
      let fns = ProverInterface
                { addCommandCallback = \smtCmd -> do
                    writeCommand cmdHandle smtCmd
                , proveFalseCallback = \g ->
                    interactiveVerifyGoal ictx g
                , proveTrueCallback = \g ->
                    interactiveVerifyGoal ictx (SMT.not g)
                , blockErrorCallback = \i a msg -> do
                    hPutStrLn stderr $ "Error: " ++ renderMCInstError funName lbl i a msg
                    modifyIORef errorCounter (+1)
                }
      let runSession = do
            mr <- try $ writeCommand cmdHandle $ SMT.setLogic SMT.allSupported
            case mr of
              Left (_e :: IOException) -> do
                hPutStrLn stderr $ "Could not start " ++ solver
                exitFailure
              Right () -> do
                writeCommand cmdHandle $ SMT.setProduceModels True
                action fns
                writeCommand cmdHandle $ SMT.exit
      seq ictx $ seq fns $ mask $ \restore -> do
        catch (restore runSession) $ \e -> do
          P.terminateProcess ph
          throwIO (e :: SomeException)
        P.terminateProcess ph
    Right _ -> do
      hPutStrLn stderr $ "Unexpected failure running " ++ solver
      exitFailure
    Left err -> do
      if isDoesNotExistError err then
        hPutStrLn stderr $ "Could not find " ++ solver ++ " executable."
       else do
        hPutStrLn stderr $ "Could not execute " ++ solver ++ "\n"
                        ++ "  " ++ show (err :: IOException)
      exitFailure

-- | This runs an action with a proof session generator, and reports
-- the final proof results.
interactiveSMTGenerator :: FilePath -- ^ Name of yaml file for error reporting purposes.
                        -> FilePath -- ^ Command line for running SMT solver
                        -> [String] -- ^ Arguments
                        -> IO ProverSessionGenerator
interactiveSMTGenerator annFile cmd cmdArgs = do
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
        let verSuccess = errorCnt == 0 && verCnt == allCnt
        if verSuccess then
          hPutStrLn stdout "Verification succeeded."
         else
          hPutStrLn stdout "Verification failed."
        hPutStrLn stdout $ printf "Verified %s/%s goal(s)." (show verCnt) (show allCnt)
        when (errorCnt > 0) $ do
          hPutStrLn stdout $ printf "Encountered %s error(s)." (show errorCnt)
        unless verSuccess exitFailure

  pure $! PSGen { blockCallbacks =
                    newInteractiveSession annFile cmd cmdArgs
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
                -> BlockLabel -- ^ Block label
                -> (ProverInterface -> IO a)
                -> IO a
exportCallbacks outDir fn lbl action = do
  goalCounter <- newIORef 0
  cmdRef <- newIORef mempty
  action $! ProverInterface
    { addCommandCallback = \(SMT.Cmd cmd) -> do
        modifyIORef' cmdRef $ \s -> s <> cmd <> "\n"
    , proveFalseCallback = \p msg ->
        exportCheckSatProblem outDir fn (ppBlock lbl) goalCounter cmdRef p msg
    , proveTrueCallback = \p msg ->
        exportCheckSatProblem outDir fn (ppBlock lbl) goalCounter cmdRef (SMT.not p) msg
    , blockErrorCallback = \i a msg ->
        hPutStrLn stderr $ "Error: " ++ renderMCInstError fn lbl i a msg
    }

runBlockVCG :: Ann.FunctionAnn -- ^ Annotations for the function we are verifying.
            -> ReachableBlockAnnMap
            -- ^ Annotations on blocks.
            -> BlockLabel -- ^ Label of first block
            -> BlockLabel -- ^ Label of this block.
            -> Ann.ReachableBlockAnn -- ^ Annotations for the block we are verifying.
            -> BlockVCG ()
            -> ModuleVCG ()
runBlockVCG funAnn blkMap firstLabel lbl blockAnn action = do
  let isFirstBlock = firstLabel == lbl
  modCtx <- ask
  let mem = moduleMem modCtx
  thisSegOff <- do
    let absAddr = fromIntegral (Ann.blockAddr blockAnn)
    case resolveAbsoluteAddr mem absAddr of
      Just o -> pure o
      Nothing -> blockError (Ann.llvmFunName funAnn) lbl (BlockAddrInvalid absAddr)

  gen <- asks proverGen
  liftIO $ blockCallbacks gen (Ann.llvmFunName funAnn) lbl $ \prover -> do
    let blockStart = Ann.blockAddr blockAnn
    let sz = Ann.blockCodeSize blockAnn
    let blockMap :: Map (MemSegmentOff 64) Ann.MemoryAnn
        blockMap = Map.fromList
          [ (segOff, Ann.eventInfo e)
          | e <- Ann.mcMemoryEvents blockAnn
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
                              , llvmFunName = Ann.llvmFunName funAnn
                              , funBlkAnnotations = blkMap
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
      mcMemDecls (Ann.pageSize ann) (Ann.stackGuardPageCount ann) (Map.elems (Ann.blockAllocas blockAnn))
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
                          , mcPendingAllocaOffsetMap = Ann.blockAllocas blockAnn
                          , llvmInstIndex = 0
                          , activeAllocaSet = Set.empty
                          }
    seq ctx $ seq s $ unBlockVCG action ctx (\() _ -> pure ()) s

data VerificationMode
   = DefaultMode
   | ExportMode !FilePath
   | RunSolver !FilePath ![String]

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
      case words cmdline of
        [] -> throwError "Expected command line argument."
        (solver:solverArgs) ->
          parseArgs rest $ args { requestedMode = RunSolver solver solverArgs }
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
  vcgArgs <- getArgs
  let initVCG = VCGArgs { reoptAnnotationsPath = Nothing, requestedMode = DefaultMode }
  args <-
    case runExcept (parseArgs vcgArgs initVCG) of
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
        case Aeson.iparse Ann.parseAnnotations jsonConfig of
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
      let cmdArgs = ["--lang=smt2", "--dedicated-eqrange-quant", "--incremental"]
      psGen <- interactiveSMTGenerator annFile "cvc4" cmdArgs
      pure (cfg, psGen)
    RunSolver cmd cmdArgs -> do
      psGen <- interactiveSMTGenerator annFile cmd cmdArgs
      pure (cfg, psGen)

standaloneGoalFilename :: String -- ^ Name of function to verify
                       -> String  -- ^ Pretty printed version of block label.
                       -> Natural -- ^ Index of goal to discharge within block
                       -> FilePath
standaloneGoalFilename fn lbl i = fn ++ "_" ++ lbl ++ "_" ++ show i ++ ".smt2"

-- | Maps between LLVM argument and machine code name.
data LLVMMCArgBinding
   = LLVMMCArgBinding { llvmArgName :: !Ident
                      , argSMTSort :: !SMT.Sort
                      , argMCReg   :: !(Some X86Reg)
                      }

-- | Define LLVM arguments in terms of the function start value of
-- machine code registers.
defineArgBinding :: LLVMMCArgBinding -> BlockVCG ()
defineArgBinding b = do
  let nm = llvmArgName b
  case argMCReg b of
    Some r ->
      addCommand $ SMT.defineFun (identVar nm) [] (argSMTSort b) (functionStartRegValue r)

-- | Define LLVM arguments in terms of the function start value of
-- machine code registers.
parseLLVMArgs :: FunctionName -- ^ Name of function for error purposes.
              -> [LLVMMCArgBinding]
              -> [Typed Ident]
              -> [X86Reg (M.BVType 64)] -- ^ Remaining registers for arguments.
              -> ModuleVCG [LLVMMCArgBinding]
parseLLVMArgs _fnm prev [] _x86Regs =
  pure (reverse prev)
parseLLVMArgs fnm prev (Typed (PrimType (Integer 64)) val : rest) x86Regs =
  case x86Regs of
    [] ->
      functionError fnm $
        SomeFunctionError (printf "Maximum of %d i64 arguments supported." (length x86ArgGPRegs))
    (reg:restRegs) -> do
      let binding = LLVMMCArgBinding { llvmArgName = val
                                     , argSMTSort = SMT.bvSort 64
                                     , argMCReg = Some reg
                                     }
      seq binding $ parseLLVMArgs fnm (binding:prev) rest restRegs
parseLLVMArgs fnm _ (Typed tp val : _rest) _x86Regs = do
  functionError fnm (FunctionArgTypeUnsupported val tp)

-- | Return true if the allocations overlap in memory.
allocaOverlap :: Ann.AllocaAnn -> Ann.AllocaAnn -> Bool
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
allocaOverlaps :: [Ann.AllocaAnn] -- ^ Existing overlaps to check against.
               -> [Ann.AllocaAnn] -- ^ Unprocessed overlaps.
               -> [(Ann.AllocaAnn, Ann.AllocaAnn)]
allocaOverlaps _prev [] = []
allocaOverlaps prev (h:r) =
  [ (x,h) | x <- reverse (filter (`allocaOverlap` h) prev) ]
  ++ allocaOverlaps (h:prev) r


-- | Check for allocations that overlap in machine code.
checkAllocaOverlap :: BlockLabel -- ^ Label for this block.
                   -> [Ann.AllocaAnn]
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

-- | Verify c over LLVM stmts
--
-- Note. This is written to take a function rather than directly call
-- @stepNextStmtg@ so that the call stack is cleaner.
checkEachStmt :: [L.Stmt] -> BlockVCG ()
checkEachStmt [] = do
  error $ "We have reached end of LLVM events without a block terminator."
checkEachStmt (stmt:stmts) = do
  setCurrentLLVMStmt stmt
  continue <- stepNextStmt stmt
  modify' $ \s -> s { llvmInstIndex = llvmInstIndex s + 1 }
  if continue then
    checkEachStmt stmts
   else
    unless (null stmts) $ error "Expected return to be last LLVM statement."

$(pure [])

llvmTypeToExprType :: Type -> Maybe Ann.ExprType
llvmTypeToExprType (L.PrimType  (L.Integer lw))
  | lw > 0 = Just (Ann.BVType (fromIntegral lw))
llvmTypeToExprType (L.PtrTo _) = Just (Ann.BVType 64)
llvmTypeToExprType _ = Nothing

$(pure [])

-- | Verify a block satisfies its specification.
verifyBlock :: HasCallStack
            => Ann.FunctionAnn -- ^ Annotations for function
            -> [LLVMMCArgBinding]
            -> ReachableBlockAnnMap
            -- ^ Annotations on blocks.
            -> BlockLabel -- ^ Label of first block.
            -> AnnotatedBlock
            -> ModuleVCG ()
verifyBlock funAnn argBindings blkMap firstLabel bAnn = do
  let lbl = abLbl bAnn
  let mblockAnn = abAnn bAnn
  let stmts = abStmts bAnn
  -- Get annotations for this block
  case mblockAnn of
    -- We only need to verify unreachable blocks are not reachable."
    Ann.UnreachableBlock -> do
      pure ()
    Ann.ReachableBlock blockAnn -> do
      -- Check allocations do not overlap with each other.
      checkAllocaOverlap lbl (Map.elems (Ann.blockAllocas blockAnn))
      -- Start running verification condition generator.
      runBlockVCG funAnn blkMap firstLabel lbl blockAnn $ do
        -- Add LLVM declarations for all existing allocations.
        forM_ (Ann.blockAllocas blockAnn) $ \a  -> do
          when (Ann.allocaExisting a) $ do
            allocaDeclarations (Ann.allocaIdent a) (bvdecimal (toInteger (Ann.allocaSize a)) 64)
        -- Declare memory
        addCommand $ SMT.declareConst (memVar 0) memSort
        -- Declare constant representing where we return to.
        defineVarFromReadMCMem "return_addr" stackHighTerm addrSupportedMemType
        -- Declare LLVM arguments in terms of Macaw registers at function start.
        mapM_ defineArgBinding argBindings
        -- Declare phi variables
        let declarePhiVar nm (tp, _) next = do
              case asSMTSort tp of
                Nothing -> do
                  error $ printf "The type %s of variable %s is not supported."
                                       (show (L.ppType tp)) (Text.unpack nm)
                Just s -> do
                  addCommand $ SMT.declareConst (llvmVar nm) s
                  next
        HMap.foldrWithKey declarePhiVar (pure ()) (abPhiVarMap bAnn)
        -- Assume preconditions
        do regs <- gets mcCurRegs
           mem  <- getMCMem
           forM_ (Ann.blockPreconditions blockAnn) $ \p -> do
             addAssert (evalPrecondition (varTerm . llvmVar) regs mem p)
        -- Start processing LLVM statements
        checkEachStmt stmts

$(pure [])

-- | Extract the phi statements from the list of statements, returning
-- either the name of the variable and type that could not be interpreted
-- or a map from from variable names to their types.
extractPhiStmtVars :: [(Ident, Type, Map BlockLabel L.Value)]
                    -> [L.Stmt]
                    -> ( [(Ident, Type, Map BlockLabel L.Value)]
                       , [L.Stmt])
extractPhiStmtVars prev (L.Result nm (Phi tp vals) _:rest) =
  let valMap = Map.fromList [ (lbl, v) | (v,lbl) <- vals ]
   in extractPhiStmtVars ((nm, tp, valMap):prev) rest
extractPhiStmtVars prev rest = (prev, rest)

$(pure [])

-- | Parse all blocks in a module.
parseBlockFn :: FunctionName
             -> HMap.HashMap Text Aeson.Object
             -> BasicBlock
             -> ModuleVCG AnnotatedBlock
parseBlockFn fnm blockMap b = do
  let Just lbl = bbLabel b
  let (phiVarList, llvmStmts) = extractPhiStmtVars [] (bbStmts b)

  let parseLLVMVar :: (Ident, Type, a) -> ModuleVCG (Text, Ann.ExprType)
      parseLLVMVar (Ident nm, tp, _) =
        case llvmTypeToExprType tp of
          Just etp ->
            pure (Text.pack nm, etp)
          Nothing ->
            blockError fnm lbl $ BlockUnsupportedPhiVarType (Ident nm) tp

  llvmVarMap <- HMap.fromList <$> traverse parseLLVMVar phiVarList

  o <- case HMap.lookup (Text.pack (ppBlock lbl)) blockMap of
         Just o ->
           pure o
         Nothing ->
           blockError fnm lbl BlockMissingAnnotations
  case Aeson.parse (Ann.parseJSONBlockAnn llvmVarMap) o of
    Aeson.Error msg ->
      blockError fnm lbl (BlockAnnParseFailure msg)
    Aeson.Success a -> do
      pure $! AnnotatedBlock { abAnn = a
                             , abLbl = lbl
                             , abPhiVarMap = HMap.fromList
                                             [ (Text.pack nm, (tp, m))
                                             | (L.Ident nm, tp, m) <- phiVarList
                                             ]
                             , abStmts = llvmStmts
                             }

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
  let fnm :: FunctionName
      fnm = Ann.llvmFunName funAnn
  vcgLog $ "Analyzing " ++ fnm

  lFun <-
    case getDefineByName lMod fnm of
      Just f ->
        pure f
      Nothing ->
        functionError fnm FunctionNotFound

  argBindings <- parseLLVMArgs fnm [] (L.defArgs lFun) x86ArgGPRegs

  blockEntries <- forM (Ann.blocks funAnn) $ \blockAnn -> do
    case HMap.lookup "label" blockAnn of
      Just (Aeson.String lbl) -> pure (lbl, blockAnn)
      Just _ -> functionError fnm "Block annotation labels must be strings."
      Nothing -> functionError fnm "Block annotations must contain \"label\" fields."

  let blockMap = HMap.fromList (V.toList blockEntries)

  blks <- traverse (parseBlockFn fnm blockMap) (defBody lFun)
  let blkMap = HMap.fromList [ (ppBlock (abLbl ab), ab) | ab <- blks ]
  case defBody lFun of
    [] ->
      functionError fnm FunctionMissingEntryBlock
    firstBlock:_ -> do
      let Just entryLabel = bbLabel firstBlock
      do firstBlockAnn <-
           case findBlock blkMap entryLabel of
             Just (Ann.ReachableBlock b, _) ->
               pure b
             Just (Ann.UnreachableBlock, _) -> do
               functionError fnm FunctionEntryUnreachable
             Nothing ->
               blockError fnm entryLabel BlockMissingAnnotations
         let Right addr = getMCAddrOfLLVMFunction (symbolAddrMap modCtx) fnm
         when (toInteger addr /= toInteger (Ann.blockAddr firstBlockAnn)) $ do
           moduleThrow $ printf "%s annotations list address of %s; symbol table reports address of %s."
                               fnm (show (Ann.blockAddr firstBlockAnn)) (show addr)
      -- Verify the blocks.
      forM_ blks $ \ab -> do
        moduleCatch $ verifyBlock funAnn argBindings blkMap entryLabel ab

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
  -- Get arguments to main
  (ann, gen) <- withVCGArgs
  -- Load Elf file and emit warnings
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
  -- print out results
  errorCnt <- readIORef errorRef
  if errorCnt > 0 then do
    hPutStrLn stderr "Errors during verification."
    exitFailure
   else
    sessionComplete gen
