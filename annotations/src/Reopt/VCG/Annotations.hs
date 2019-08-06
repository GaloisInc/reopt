{-|

This module defines the data structures used to representation
annotation information, and routines for serializing and deserializing
to JSON.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.VCG.Annotations
  ( ReoptVCGAnnotations(..)
  , FunctionAnn(..)
  , BlockAnn(..)
  , AllocaInfo(..)
  , AllocaName(..)
  , BlockEvent(..)
  , MemoryAccessType(..)
  , Expr(..)
  , BlockVar(..)
  , calleeSavedGPRegs
  ) where

import           Control.Monad
import           Data.Aeson.Types ((.:), (.:?), (.!=), (.=), object)
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HSet
import qualified Data.Scientific as S
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import           Data.Word
import qualified Flexdis86 as F
import           GHC.Generics
import           GHC.Natural
import           Numeric (showHex)

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

-- | @optValList nm l@ generates a binding from @nm@ to @l@ if @l@ is
-- non-empty, and leaves it blank otherwise.
--
-- This is useful for emitting lists that default to empty.
optValList :: Aeson.ToJSON a => Text -> [a] -> [(Text,Aeson.Value)]
optValList _ [] = []
optValList nm l = [nm .= l]

------------------------------------------------------------------------
-- AllocaName

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
    | Just off <- S.toBoundedInteger n :: Maybe Word64 =
        pure $ AllocaName (Text.pack (show off))
  parseJSON v =
    fail $ "Allocation name Expected integer or string, not " ++ show v

instance Aeson.ToJSON AllocaName where
  toJSON (AllocaName nm) = Aeson.String nm

------------------------------------------------------------------------
-- AllocaInfo

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
    when (sz > o) $
      fail $ "Allocation size " ++ show sz ++ " must not be greater than offset " ++ show o ++ "."
    pure AllocaInfo { allocaName = nm
                    , allocaBinaryOffset = o
                    , allocaSize = sz
                    , allocaExisting = existing
                    }

instance Aeson.ToJSON AllocaInfo where
  toJSON a = Aeson.object [ "name" .= allocaName a
                          , "offset" .= allocaBinaryOffset a
                          , "size"   .= allocaSize a
                          , "existing" .= allocaExisting a
                          ]

------------------------------------------------------------------------
-- BlockEventType

data MemoryAccessType
   = BinaryOnlyAccess
     -- ^ The instruction at the address updates the binary
     -- stack, but does not affect LLVM memory.
   | JointStackAccess !AllocaName
     -- ^ The instructions at the address access the LLVM allocation
     -- associated with the given name.
   | HeapAccess
     -- ^ There is an access to heap memory.
  deriving (Show)

parseMemoryAccessType :: Aeson.Object -> Aeson.Parser MemoryAccessType
parseMemoryAccessType v = do
  tp <- v .: "type"
  case (tp :: Text) of
    "binary_only_access" -> pure BinaryOnlyAccess
    "joint_stack_access" -> do
      JointStackAccess <$> v .: "alloca"
    "heap_access" -> pure HeapAccess
    _ -> fail "Unexpected alloca type"

renderMemoryAccessType :: MemoryAccessType -> [(Text, Aeson.Value)]
renderMemoryAccessType BinaryOnlyAccess = ["type" .= Aeson.String "binary_old_access"]
renderMemoryAccessType (JointStackAccess a) = [ "type" .= Aeson.String "joint_stack_access"
                                              , "alloca" .= a
                                              ]
renderMemoryAccessType HeapAccess = ["type" .= Aeson.String "heap_access" ]

------------------------------------------------------------------------
-- BlockEvent

type MCAddr = Word64

newtype JSONAddr = JSONAddr { mcAddr :: MCAddr }

instance Aeson.FromJSON JSONAddr where
  parseJSON (Aeson.String txt)
    | Just num <- Text.stripPrefix "0x" txt
    , Right (w,"") <- Text.hexadecimal num
    , w <= toInteger (maxBound :: Word64) = do
        pure $! JSONAddr (fromInteger w)
  parseJSON (Aeson.Number v)
    | S.isInteger v
    , Just n <- S.toBoundedInteger v =
        pure $! JSONAddr n
  parseJSON v = do
    fail $ "Address expected a 64-bit number, received " ++ show v

instance Aeson.ToJSON JSONAddr where
  toJSON (JSONAddr a) = Aeson.String ("0x" <> Text.pack (showHex a ""))

-- | Annotes an event at a given address.
data BlockEvent = BlockEvent
  { eventAddr :: !MCAddr
  , eventInfo :: !MemoryAccessType
  }
  deriving (Show)

-- | Lift of fields
blockEventFields :: FieldList
blockEventFields = fields ["addr", "type", "alloca"]

instance Aeson.FromJSON BlockEvent where
  parseJSON = withFixedObject "BlockEvent" blockEventFields $ \v -> do
    addr <- mcAddr <$> v .: "addr"
    info <- parseMemoryAccessType v
    pure $ BlockEvent { eventAddr = addr
                      , eventInfo = info
                      }

instance Aeson.ToJSON BlockEvent where
  toJSON e = object
    $  ["addr" .= JSONAddr (eventAddr e) ]
    ++ renderMemoryAccessType (eventInfo e)

------------------------------------------------------------------------
-- BlockVar

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
    pure (MCStack a w, BVType w)

  fromExpr (Atom nm)
    | Just (v,tp) <- HMap.lookup nm blockVarNameMap = Right (v, tp)
  fromExpr s =
    Left $ "Could not interpret " ++ ppSExpr s ""

  encodeVar StackHigh = "stack_high"
  encodeVar (InitGPReg64 r) = fromString (show r)
  encodeVar (FnStartGPReg64 r) = fromString $ "fnstart_ " <> show r
  encodeVar (MCStack e w) = encodeList [encodeExpr e, fromString (show w)]

------------------------------------------------------------------------
-- JSONExpr

-- | An SMT expression that can be rendered into JSON.
newtype JSONExpr = JSONExpr { jsonExpr :: Expr BlockVar }

instance Aeson.FromJSON JSONExpr where
  parseJSON (Aeson.String s) =
    case fromText s of
      Left msg -> fail msg
      Right e -> pure (JSONExpr e)
  parseJSON _ = error "Precondition must be a string."

instance Aeson.ToJSON JSONExpr where
  toJSON (JSONExpr e) = Aeson.String (exprToText e)

------------------------------------------------------------------------
-- BlockAnn

-- | Annotations that relate a LLVM block to a contiguous sequence of
-- machine code instrutions.
data BlockAnn = BlockAnn
  { blockLabel :: !String
    -- ^ LLVM label of block
  , blockAddr :: !MCAddr
    -- ^ Address of start of block in machine code
  , blockCodeSize :: !Word64
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

instance Aeson.FromJSON BlockAnn where
  parseJSON = withFixedObject "block" blockInfoFields $ \v -> do
    lbl  <- v .: "label"
    addr <- mcAddr <$> v .: "addr"
    sz   <- mcAddr <$> v .: "size"
    when (addr + sz < addr) $ do
      fail $ "Expected end of block computation to not overflow."
    x87Top  <- v .:? "x87_top"    .!= 7
    dfFlag  <- v .:? "df_flag"    .!= False
    preconditions <- v .:? "preconditions" .!= []
    allocas <- v .:? "allocas"    .!= []
    events  <- v .:? "events"     .!= []
    pure BlockAnn { blockLabel = lbl
                  , blockAddr  = addr
                  , blockCodeSize = sz
                  , blockX87Top    = x87Top
                  , blockDFFlag    = dfFlag
                  , blockPreconditions = jsonExpr <$> preconditions
                  , blockAllocas = allocas
                  , blockEvents = events
                  }

instance Aeson.ToJSON BlockAnn where
  toJSON blk =
    object
    $ [ "label"  .= blockLabel blk
      , "addr"   .= JSONAddr (blockAddr blk)
      , "stack_size" .= blockCodeSize blk
      , "x87_top"    .= blockX87Top blk
      , "df_flag"    .= blockDFFlag blk
      ]
    <> optValList "preconditions" (JSONExpr <$> blockPreconditions blk)
    <> optValList "allocas"       (blockAllocas blk)
    <> optValList "events"        (blockEvents  blk)

------------------------------------------------------------------------
-- FunctionAnn

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

instance Aeson.ToJSON FunctionAnn where
  toJSON fun =
    object [ "llvm_name"  .= llvmFunName fun
           , "stack_size" .= stackSize fun
           , "blocks"     .= HMap.elems (blocks fun)
           ]

------------------------------------------------------------------------
-- Module annotations

data ReoptVCGAnnotations = ReoptVCGAnnotations
  { llvmFilePath :: FilePath
    -- ^ Path to LLVM .bc or .ll file path
  , binFilePath    ::  String
    -- ^ Binary file path that will be analyzed by Macaw
  , functions :: [FunctionAnn]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON ReoptVCGAnnotations where
  parseJSON (Aeson.Object o) = do
    llvmPath <- o .: "llvm_path"
    binPath  <- o .: "binary_path"
    funs     <- o .: "functions"
    pure $! ReoptVCGAnnotations { llvmFilePath = llvmPath
                          , binFilePath = binPath
                          , functions = funs
                          }
  parseJSON _ =
    fail $ "Expected an object for the meta config."


instance Aeson.ToJSON ReoptVCGAnnotations where
   toJSON ann =
     object [ "llvm_path"   .= llvmFilePath ann
            , "binary_path" .= binFilePath ann
            , "functions"   .= functions ann
            ]
