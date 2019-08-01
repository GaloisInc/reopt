{-|

This module defines the data structures used to representation
annotation information, and routines for serializing and deserializing
to JSON.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.VCG.Annotations
  ( MetaVCGConfig(..)
  , FunctionAnn(..)
  , BlockAnn(..)
  , AllocaInfo(..)
  , AllocaName(..)
  , BlockEvent(..)
  , BlockEventInfo(..)
  , Expr(..)
  , BlockVar(..)
  , calleeSavedGPRegs
  ) where

import           Control.Monad
import           Data.Aeson.Types ((.:), (.:?), (.!=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HSet
import           Data.Scientific (toBoundedInteger)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import qualified Flexdis86 as F
import           GHC.Generics
import           GHC.Natural

import           Reopt.VCG.SMTParser

------------------------------------------------------------------------
-- JSON utilities

-- | A list of valid fields for an object.
type FieldList = HashSet Text

-- | Create a field list from a list
fields :: [Text] -> FieldList
fields = HSet.fromList

-- | Parse a YAML and fail if there are any fields not in the set.
withFixedObject :: String
                -> FieldList
                -> (Aeson.Object -> Aeson.Parser a)
                -> Aeson.Value
                -> Aeson.Parser a
withFixedObject nm flds f (Aeson.Object o) =
  case HMap.foldrWithKey badFields [] o of
    [] -> f o
    l -> fail $ "Unexpected fields in " ++ nm ++ ": " ++ show l
  where badFields :: Text -> Aeson.Value -> [Text] -> [Text]
        badFields fld _ l =
          if HSet.member fld flds then
            l
           else
            fld:l
withFixedObject _ _ _ _ = fail "Expected an object."

------------------------------------------------------------------------
-- AllocaInfo

-- | Identifier associated with an LLVM allocation.
newtype AllocaName = AllocaName { allocaNameText :: Text }
  deriving (Eq, Ord)

instance IsString AllocaName where
  fromString = AllocaName . Text.pack

instance Show AllocaName where
  show (AllocaName nm) = Text.unpack nm

instance Aeson.FromJSON AllocaName where
  parseJSON (Aeson.String nm) = pure $ AllocaName nm
  parseJSON (Aeson.Number n)
    | Just off <- toBoundedInteger n :: Maybe Word64 =
        pure $ AllocaName (Text.pack (show off))
  parseJSON v =
    fail $ "Allocation name Expected integer or string, not " ++ show v

-- | Annotes an event at a given address.
data AllocaInfo = AllocaInfo
  { allocaName :: !AllocaName
    -- ^ Name of allocation.
  , allocaBinaryOffset :: !Natural
    -- ^ Number of bytes from start of alloca to offset of stack
    -- pointer in machine code.
  , allocaSize :: !Natural
    -- ^ Size of allocation in bytes.
  , allocaExisting :: !Bool
    -- ^ Stores true if the allocation already exists at this block.
    -- The default is true, so we only need to assign this to false.
  }
  deriving (Show)

allocaInfoFields :: FieldList
allocaInfoFields = fields ["name", "offset", "size", "existing"]

instance Aeson.FromJSON AllocaInfo where
  parseJSON = withFixedObject "AllocaInfo" allocaInfoFields $ \v -> do
    nm <- v .: "name"
    o <- v .: "offset"
    sz <- v .: "size"
    existing <- (v .:? "existing") .!= True
    when (sz > o) $ fail $ "Allocation size " ++ show sz ++ " must not be greater than offset " ++ show o ++ "."
    pure AllocaInfo { allocaName = nm
                    , allocaBinaryOffset = o
                    , allocaSize = sz
                    , allocaExisting = existing
                    }

------------------------------------------------------------------------
-- BlockEventType

data BlockEventInfo
   = BinaryOnlyAccess
     -- ^ The instruction at the address updates the binary
     -- stack, but does not affect LLVM memory.
   | JointStackAccess !AllocaName
     -- ^ The instructions at the address access the LLVM allocation
     -- associated with the given name.
   | HeapAccess
     -- ^ There is an access to heap memory.
  deriving (Show)

------------------------------------------------------------------------
-- BlockEvent

type MCAddr = Word64


-- | Annotes an event at a given address.
data BlockEvent = BlockEvent
  { eventAddr :: !MCAddr
  , eventInfo :: !BlockEventInfo
  }
  deriving (Show)

-- | Lift of fields
blockEventFields :: FieldList
blockEventFields = fields ["addr", "type", "alloca"]

instance Aeson.FromJSON BlockEvent where
  parseJSON = withFixedObject "BlockEvent" blockEventFields $ \v -> do
    addr <- v .: "addr"
    tp <- v .: "type"
    info <-
      case (tp :: Text) of
        "binary_only_access" -> pure BinaryOnlyAccess
        "joint_stack_access" -> do
          JointStackAccess <$> v .: "alloca"
        "heap_access" -> pure HeapAccess
        _ -> fail "Unexpected alloca type"
    pure $ BlockEvent { eventAddr = addr
                      , eventInfo = info
                      }

------------------------------------------------------------------------
-- BlockAnn

-- | This is the list of callee saved registers.
calleeSavedGPRegs :: [F.Reg64]
calleeSavedGPRegs = [ F.RBP, F.RBX, F.R12, F.R13, F.R14, F.R15 ]

-- | A variable that may appear in a block invariant.
data BlockVar
   = StackHigh
     -- ^ Denotes the high address on the stack.
     --
     -- This is the address the return address is stored at.
   | InitGPReg64 !F.Reg64
     -- ^ Denotes a 64-bit general purpose register a
   | FnStartGPReg64 !F.Reg64
     -- ^ Denotes the value of a general purpose when the function starts.
     --
     -- Note. We do not support all registers here, only the registers
     -- in `calleeSavedGPRegs`
   | MCStack !(Expr BlockVar) !Natural
     -- ^ @MCStack a w@ denotes @w@-bit value stored at the address @a@.
     --
     -- The width @w@ should be @8@, @16@, @32@, or @64@.
     --
     -- Our memory model only tracks the mc-only variables, so if the
     -- address is not a stack-only variable, then the value just
     -- means some arbitrary value.
  deriving (Show)

-- | Hashmap that maps constants to their block var.
blockVarNameMap :: HMap.HashMap Text (BlockVar, ExprType)
blockVarNameMap = HMap.fromList $
  [ (Text.pack (show r), (InitGPReg64 r, BVType 64))
  | r <- F.Reg64 <$> [0..15]
  ]
  ++ [("stack_high", (StackHigh, BVType 64))]
  ++ [ (Text.pack ("fnstart_" ++ show r), (FnStartGPReg64 r, BVType 64))
     | r <- calleeSavedGPRegs
     ]

instance IsExprVar BlockVar where
  fromExpr (List [Atom "mcstack", sa, sw]) = do
    (a, tp) <- evalExpr sa
    when (tp /= BVType 64) $ fail "Expected 64-bit address."
    w <- case sw of
           List [Atom "_", Atom "BitVec", Number w] | w `elem` [8,16,32,64] -> pure w
           _ -> fail $ "mcstack could not interpet memory type."
    pure (Var (MCStack a w), BVType w)

  fromExpr (Atom nm)
    | Just (v,tp) <- HMap.lookup nm blockVarNameMap = Right (Var v, tp)
  fromExpr s =
    Left $ "Could not interpret " ++ ppSExpr s ""

-- | Our VCG supports cases where each LLVM block corresponds to a
-- contiguous range of instructions.
data BlockAnn = BlockAnn
  { blockLabel :: !String
    -- ^ LLVM label of block
  , blockAddr :: !MCAddr
    -- ^ Address of start of block in machine code
  , blockCodeSize :: !Integer
    -- ^ Number of bytes in block
  , blockX87Top  :: !Int
    -- ^ The top of x87 stack (empty = 7, full = 0)
  , blockDFFlag  :: !Bool
    -- ^ The value of the DF flag (default = False)
  , blockPreconditions :: ![Expr BlockVar]
    -- ^ List of preconditions for block.
  , blockAllocas :: ![AllocaInfo]
    -- ^ Maps LLVM allocations to an offset of the stack where it
    -- starts.
  , blockEvents :: ![BlockEvent]
    -- ^ Annotates events within the block.
  }
  deriving (Show, Generic)


blockInfoFields :: FieldList
blockInfoFields =
  fields ["label", "addr", "size", "x87_top", "df_flag", "allocas", "preconditions", "events"]

parsePred :: Text -> Aeson.Parser (Expr BlockVar)
parsePred t =
  case readPred t of
    Left msg -> fail msg
    Right e -> pure e

instance Aeson.FromJSON BlockAnn where
  parseJSON = withFixedObject "block" blockInfoFields $ \v -> do
    lbl  <- v .: "label"
    addr <- v .: "addr"
    sz   <- v .: "size"
    x87Top  <- v .:? "x87_top"    .!= 7
    dfFlag  <- v .:? "df_flag"    .!= False
    precondStrs <- v .:? "preconditions" .!= []
    preconditions <- traverse parsePred precondStrs
    allocas <- v .:? "allocas"    .!= []
    events  <- v .:? "events"     .!= []
    pure BlockAnn { blockLabel = lbl
                  , blockAddr  = addr
                  , blockCodeSize = sz
                  , blockX87Top    = x87Top
                  , blockDFFlag    = dfFlag
                  , blockPreconditions = preconditions
                  , blockAllocas = allocas
                  , blockEvents = events
                  }

data FunctionAnn = FunctionAnn
  { llvmFunName    :: !String
    -- ^ LLVM function name
  , stackSize :: !Natural
    -- ^ Number of bytes in binary stack size.
  , blocks :: !(HMap.HashMap String BlockAnn)
    -- ^ Maps LLVM labels to the block associated with that label.
  }
  deriving (Show)

functionInfoFields :: FieldList
functionInfoFields = fields ["llvm_name", "stack_size", "blocks"]

instance Aeson.FromJSON FunctionAnn where
  parseJSON = withFixedObject "function" functionInfoFields $ \v -> do
    fnm <- v .: "llvm_name"
    sz  <- v .: "stack_size"
    bl <- v .: "blocks"
    pure $! FunctionAnn { llvmFunName = fnm
                        , stackSize = sz
                        , blocks = HMap.fromList [ (blockLabel b, b) | b <- bl ]
                        }

data MetaVCGConfig = MetaVCGConfig
  { llvmBCFilePath :: FilePath
    -- ^ LLVM .bc file path
  , binFilePath    ::  String
    -- ^ Binary file path that will be analyzed by Macaw
  , functions :: [FunctionAnn]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MetaVCGConfig
