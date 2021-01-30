{-|

This module defines the data structures used to representation
annotation information, and routines for serializing and deserializing
to JSON.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.VCG.Annotations
  ( ModuleAnnotations(..)
  , FunctionAnn(..)
  , BlockAnn(..)
  , MCAddr
  , ReachableBlockAnn(..)
  , AllocaAnn(..)
  , LocalIdent(..)
  , MCMemoryEvent(..)
  , MemoryAnn(..)
  , SMTP.Expr(..)
  , SMTP.exprToText
  , BlockVar(..)
  , parseAnnotations
  , LLVMVarMap
  , SMTP.ExprType(..)
  , parseJSONBlockAnn
  , blockAnnToJSON
  , x86ArgGPRegs
  , calleeSavedGPRegs
  ) where

import           Control.Monad
import           Data.Aeson.Types ((.:), (.:!), (.!=), (.=), object)
import qualified Data.Aeson.Types as Aeson
import           Data.Bits
import qualified Data.HashMap.Strict as HMap
import qualified Data.Macaw.X86.X86Flag as M
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as S
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Vector as V
import           Data.Word
import qualified Flexdis86 as F
import           GHC.Generics
import           GHC.Natural
import           Numeric (showHex)

import           Reopt.VCG.SMTParser as SMTP

------------------------------------------------------------------------
-- JSON utilities

-- | Parse a YAML and fail if there are any fields not in the set.
withFixedObject :: String
                -> (Aeson.Object -> Aeson.Parser a)
                -> Aeson.Value
                -> Aeson.Parser a
withFixedObject _nm f (Aeson.Object o) = f o
withFixedObject nm _ _ = fail $ "Expected an object for " ++ nm ++ "."

-- | @optValList nm l@ generates a binding from @nm@ to @l@ if @l@ is
-- non-empty, and leaves it blank otherwise.
--
-- This is useful for emitting lists that default to empty.
optValList :: Aeson.ToJSON a => Text -> [a] -> [(Text,Aeson.Value)]
optValList _ [] = []
optValList nm l = [nm .= l]

-- | @optVal nm v default@ generates a binding from @nm@ to @v@ if @v@
-- is distinct from @default@, and the empty list otherwise.
optVal :: (Eq a, Aeson.ToJSON a) => Text -> a -> a -> [(Text,Aeson.Value)]
optVal nm v d | v == d = []
              | otherwise = [nm .= v]

------------------------------------------------------------------------
-- LocalIdent

-- | A local LLVM identifier
newtype LocalIdent = LocalIdent { allocaNameText :: Text }
  deriving (Eq, Ord)

instance IsString LocalIdent where
  fromString = LocalIdent . Text.pack

instance Show LocalIdent where
  show (LocalIdent nm) = Text.unpack nm

instance Aeson.FromJSON LocalIdent where
  parseJSON (Aeson.String nm) = pure $ LocalIdent nm
  parseJSON (Aeson.Number n)
    | Just off <- S.toBoundedInteger n :: Maybe Word64 =
        pure $ LocalIdent (Text.pack (show off))
  parseJSON v =
    fail $ "Allocation name Expected integer or string, not " ++ show v

instance Aeson.ToJSON LocalIdent where
  toJSON (LocalIdent nm) = Aeson.String nm

------------------------------------------------------------------------
-- AllocaAnn

-- | Provides a mapping between LLVM alloca and machine code stack usage.
data AllocaAnn = AllocaAnn
  { allocaIdent :: !LocalIdent
    -- ^ The LLVM identifier initialized by the allocation.
  , allocaBinaryOffset :: !Natural
    -- ^ Number of bytes from start of alloca to offset of stack
    -- pointer in machine code.
    --
    -- The stack grows down, so the actual memory addresses represented
    -- are
    -- @[rsp0 - allocaBinaryOffset, rsp0 - allocaBinaryOffset + allocaSize)@
    -- where @rsp0@ denotes the value of @rsp@ when the function starts.
  , allocaSize :: !Natural
    -- ^ Size of allocation in bytes.
  , allocaExisting :: !Bool
    -- ^ Stores true if the allocation already exists at this block.
    -- The default is true, so we only need to assign this to false.
  }
  deriving (Eq, Show)

instance Aeson.FromJSON AllocaAnn where
  parseJSON = withFixedObject "AllocaAnn" $ \v -> do
    nm <- v .: "llvm_ident"
    o <- v .: "offset"
    sz <- v .: "size"
    existing <- (v .:! "existing") .!= True
    when (sz > o) $
      fail $ "Allocation size " ++ show sz ++ " must not be greater than offset " ++ show o ++ "."
    pure AllocaAnn { allocaIdent = nm
                    , allocaBinaryOffset = o
                    , allocaSize = sz
                    , allocaExisting = existing
                    }

instance Aeson.ToJSON AllocaAnn where
  toJSON a = Aeson.object [ "llvm_ident" .= allocaIdent a
                          , "offset" .= allocaBinaryOffset a
                          , "size"   .= allocaSize a
                          , "existing" .= allocaExisting a
                          ]

------------------------------------------------------------------------
-- MemoryAnn

-- | Annotation on memory address.
data MemoryAnn
   = BinaryOnlyAccess
     -- ^ The instruction at the address updates the binary
     -- stack, but does not affect LLVM memory.
   | JointStackAccess !LocalIdent
     -- ^ The instructions at the address access the LLVM allocation
     -- associated with the given name.
   | HeapAccess
     -- ^ There is an access to heap memory.
  deriving (Show)

parseMemoryAnn :: Aeson.Object -> Aeson.Parser MemoryAnn
parseMemoryAnn v = do
  tp <- v .: "type"
  case (tp :: Text) of
    "binary_only_access" -> pure BinaryOnlyAccess
    "joint_stack_access" -> JointStackAccess <$> v .: "alloca"
    "heap_access" -> pure HeapAccess
    _ -> fail "Unexpected alloca type"

renderMemoryAnn :: MemoryAnn -> [(Text, Aeson.Value)]
renderMemoryAnn BinaryOnlyAccess =
  ["type" .= Aeson.String "binary_only_access"]
renderMemoryAnn (JointStackAccess a) =
  [ "type" .= Aeson.String "joint_stack_access"
  , "alloca" .= a
  ]
renderMemoryAnn HeapAccess =
  ["type" .= Aeson.String "heap_access" ]

------------------------------------------------------------------------
-- MCAddr

-- | This represents the address of code.
--
-- For non-position independent executables, it is an absolute address.
--
-- For position independent executables and libraries, it is relative
-- to the base address.
--
-- For object files, it is the offset into the .text section.
newtype MCAddr = MCAddr { mcAddr :: Word64 }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show MCAddr where
  showsPrec _ (MCAddr w) = ("0x" ++) . showHex w

instance Aeson.FromJSON MCAddr where
  parseJSON (Aeson.String txt)
    | Just num <- Text.stripPrefix "0x" txt
    , Right (w,"") <- Text.hexadecimal num
    , w <= toInteger (maxBound :: Word64) = do
        pure $! MCAddr (fromInteger w)
  parseJSON (Aeson.Number v)
    | S.isInteger v
    , Just n <- S.toBoundedInteger v =
        pure $! MCAddr n
  parseJSON v = do
    fail $ "Address expected a 64-bit number, received " ++ show v

instance Aeson.ToJSON MCAddr where
  toJSON (MCAddr a) = Aeson.String ("0x" <> Text.pack (showHex a ""))

------------------------------------------------------------------------
-- MCMemoryEvent

-- | Annotes an event at a given address.
data MCMemoryEvent = MCMemoryEvent
  { eventAddr :: !MCAddr
    -- ^ Address in machine code where event occurs.
  , eventInfo :: !MemoryAnn
  }
  deriving (Show)

instance Aeson.FromJSON MCMemoryEvent where
  parseJSON = withFixedObject "MCMemoryEvent" $ \v -> do
    addr <- v .: "addr"
    info <- parseMemoryAnn v
    pure $ MCMemoryEvent { eventAddr = addr
                         , eventInfo = info
                         }

instance Aeson.ToJSON MCMemoryEvent where
  toJSON e = object
    $  ["addr" .= eventAddr e ]
    ++ renderMemoryAnn (eventInfo e)

------------------------------------------------------------------------
-- BlockVar

-- | General purpose registers that may be used to pass arguments.
x86ArgGPRegs :: [F.Reg64]
x86ArgGPRegs = [ F.RDI, F.RSI, F.RDX, F.RCX, F.R8, F.R9 ]

-- | This is the list of callee saved registers.
calleeSavedGPRegs :: [F.Reg64]
calleeSavedGPRegs = [ F.RBP, F.RBX, F.R12, F.R13, F.R14, F.R15 ]

-- | A variable that may appear in a block precondition.
data BlockVar
   = StackHigh
     -- ^ Denotes the high address on the stack.
     --
     -- This is the address the return address is stored at, and
     -- the curent frame.
   | InitGPReg64 !F.Reg64
     -- ^ Denotes the value of a 64-bit general purpose register
     -- at the start of the block execution.
   | InitFlagReg !M.X86Flag
     -- ^ Denotes the value of a flag register at the start of execution.
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
   | LLVMVar !Text
     -- ^ This denotes the value of an LLVM Phi variable when the
     -- block starts.
  deriving (Show)

-- | Hashmap that maps constants to their block var.
regVarMap :: HMap.HashMap Text F.Reg64
regVarMap = HMap.fromList $
  [ (Text.pack (show r), r)
  | r <- F.Reg64 <$> [0..15]
  ]

-- | Map from LLVM phi variable names to their expected tpye.
type LLVMVarMap = HMap.HashMap Text ExprType

-- | Attempt to parse a block variable from an S-expression.
fromExpr :: LLVMVarMap
            -- ^ Map from LLVM identifiers to their associated type.
         -> SExpr
         -> Either String (BlockVar, ExprType)
fromExpr llvmMap (List [Atom "mcstack", sa, sw]) = do
  (a, tp) <- evalExpr (fromExpr llvmMap)  sa
  when (tp /= BVType 64) $ Left $ "Expected 64-bit address."
  w <- case sw of
         List [Atom "_", Atom "BitVec", Number w] | w `elem` [8,16,32,64] -> pure w
         _ -> Left $ "mcstack could not interpret memory type."
  pure (MCStack a w, BVType w)
fromExpr _llvmMap (List [Atom "fnstart", regExpr]) =
  case regExpr of
    Atom regName
      | Just r <- HMap.lookup regName regVarMap ->
          pure (FnStartGPReg64 r, BVType 64)
    _ ->
      Left $ "Could not interpret register name " ++ ppSExpr regExpr ""
fromExpr llvmMap (List [Atom "llvm", llvmExpr]) =
  case llvmExpr of
    Atom llvmName
      | Just tp <- HMap.lookup llvmName llvmMap ->
          pure (LLVMVar llvmName, tp)
    _ -> Left $ "Could not interpret llvm variable " ++ ppSExpr llvmExpr "\n"
           ++ "Known variables: " ++ show (HMap.keys llvmMap)
fromExpr _llvmMap (Atom "stack_high") = Right (StackHigh, BVType 64)
fromExpr _llvmMap (Atom nm)
  | Just r <- HMap.lookup nm regVarMap = Right (InitGPReg64 r, BVType 64)
fromExpr _llvmMap s =
  Left $ "Could not interpret expression " ++ ppSExpr s ""


instance IsExprVar BlockVar where
  encodeVar StackHigh = "stack_high"
  encodeVar (InitGPReg64 r) = fromString (show r)
  encodeVar (InitFlagReg r) = fromString (show r)
  encodeVar (FnStartGPReg64 r) = encodeList ["fnstart", fromString (show r)]
  encodeVar (MCStack e w) =
    let tp = encodeList ["_", "BitVec", fromString (show w)]
     in encodeList ["mcstack", encodeExpr e, tp]
  encodeVar (LLVMVar nm) = encodeList ["llvm", sexprFromText nm]

------------------------------------------------------------------------
-- JSONExpr

-- | An SMT expression that can be rendered into JSON.
newtype JSONExpr = JSONExpr { _jsonExpr :: Expr BlockVar }

parseExpr :: LLVMVarMap -> Aeson.Value -> Aeson.Parser (Expr BlockVar)
parseExpr llvmMap (Aeson.String s) =
  case fromText (fromExpr llvmMap) s of
    Left msg -> fail msg
    Right e -> pure e
parseExpr _ _ = error "Precondition must be a string."

instance Aeson.ToJSON JSONExpr where
  toJSON (JSONExpr e) = Aeson.String (exprToText e)

------------------------------------------------------------------------
-- ReachableBlockAnn

-- | Annotations that relate a LLVM block to a contiguous sequence of
-- machine code instrutions.
data ReachableBlockAnn
  = ReachableBlockAnn
    { blockAddr :: !MCAddr
      -- ^ Address of start of block in machine code
    , blockCodeSize :: !Word64
      -- ^ Number of bytes in block
    , blockX87Top  :: !Word8
      -- ^ The top of x87 stack (empty = 7, full = 0)
    , blockDFFlag  :: !Bool
      -- ^ The value of the DF flag (default = False)
      -- | List of preconditions for block.
    , blockPreconditions :: ![Expr BlockVar]
      -- | Maps identifiers to the allocation used to initialize them.
      --
      -- The same allocations should be used across the function, but
      -- some block may not have been initialized.
    , blockAllocas :: !(Map LocalIdent AllocaAnn)
      -- | Annotates events within the block.
    , mcMemoryEvents :: ![MCMemoryEvent]
    }
  deriving (Show, Generic)

------------------------------------------------------------------------
-- BlockAnn

-- | This provides an indication of whether a block is expected to be reachable,
-- and if so, what invariants should be true.
data BlockAnn
   = ReachableBlock !ReachableBlockAnn
     -- ^ Indicates the block is reachable and provides object with other information.
   | UnreachableBlock
     -- ^ Indicates the block is unreachable.
  deriving (Show)

parseArray :: Text -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Object -> Aeson.Parser [a]
parseArray nm f o = do
  mo <- o .:! nm
  case mo of
    Nothing -> pure []
    Just v -> Aeson.withArray (Text.unpack nm) (traverse f . V.toList) v

parseJSONBlockAnn :: LLVMVarMap
                  -> Aeson.Object
                  -> Aeson.Parser BlockAnn
parseJSONBlockAnn llvmMap o = do
  reachable <- o .:! "reachable" .!= True
  case reachable of
    False -> do
      pure $! UnreachableBlock
    True -> do
      addr <- o .: "addr"
      sz   <- mcAddr <$> o .: "size"
      when (mcAddr addr + sz < mcAddr addr) $ do
        fail $ "Expected end of block computation to not overflow."
      x87Top  <- o .:! "x87_top"    .!= 7
      dfFlag  <- o .:! "df_flag"    .!= False
      preconditions <- parseArray "preconditions" (parseExpr llvmMap) o
      allocas <- o .:! "allocas"    .!= []
      mcEvents  <- o .:! "mem_events"     .!= []
      let ann = ReachableBlockAnn { blockAddr  = addr
                                  , blockCodeSize = sz
                                  , blockX87Top    = x87Top
                                  , blockDFFlag    = dfFlag
                                  , blockPreconditions = preconditions
                                  , blockAllocas =
                                      Map.fromList [ (allocaIdent a, a) | a <- allocas ]
                                  , mcMemoryEvents = mcEvents
                                  }
      pure $! ReachableBlock ann

-- | Convert a block annotation and label to JSON
blockAnnToJSON :: String -> BlockAnn -> Aeson.Object
blockAnnToJSON lbl UnreachableBlock =
  HMap.fromList $
    [ "label" .= lbl
    , "reachable" .= False
    ]
blockAnnToJSON lbl (ReachableBlock blk) =
  HMap.fromList
    $ [ "label"      .= lbl
      , "addr"       .= blockAddr blk
      , "size"       .= blockCodeSize blk
      ]
    <> optVal     "x87_top"       (blockX87Top blk) 7
    <> optVal     "df_flag"       (blockDFFlag blk) False
    <> optValList "preconditions" (JSONExpr <$> blockPreconditions blk)
    <> optValList "allocas"       (Map.elems (blockAllocas blk))
    <> optValList "mem_events"    (mcMemoryEvents blk)

------------------------------------------------------------------------
-- FunctionAnn

-- | Annotations for a function.
data FunctionAnn = FunctionAnn
  { llvmFunName :: !String
    -- ^ LLVM function name
  , blocks :: !(V.Vector Aeson.Object)
    -- ^ Maps LLVM labels to an JSON object describing information associated with
    -- that block.
  } deriving (Show)

parseJSONBlockAnnObj :: Aeson.Value
                     -> Aeson.Parser Aeson.Object
parseJSONBlockAnnObj (Aeson.Object o) = do
  pure o
parseJSONBlockAnnObj _ =
  fail "block annotation expects object."

parseFunctionAnn :: Aeson.Value
                 -> Aeson.Parser FunctionAnn
parseFunctionAnn (Aeson.Object v) = do
  fnm <- v .: "llvm_name"
  bl <- Aeson.withArray "blocks" (traverse parseJSONBlockAnnObj . V.toList) =<< v .: "blocks"
  pure $! FunctionAnn { llvmFunName = fnm
                      , blocks = V.fromList bl
                      }
parseFunctionAnn _ =
  fail $ "Function annotation expected a JSON object."

instance Aeson.ToJSON FunctionAnn where
  toJSON fun =
    object [ "llvm_name"  .= llvmFunName fun
           , "blocks"     .= Aeson.Array (Aeson.Object <$> blocks fun)
           ]
------------------------------------------------------------------------
-- Module annotations

data ModuleAnnotations = ModuleAnnotations
  { llvmFilePath :: FilePath
    -- ^ Path to LLVM .bc or .ll file path
  , binFilePath    ::  String
    -- ^ Binary file path that will be analyzed by Macaw.
  , pageSize :: !Natural
    -- ^ The number of bytes in a page (must be a power of 2)
  , stackGuardPageCount :: !Natural
    -- ^ The number of unallocated pages beneath the stack.
  , functions :: [FunctionAnn]
  }
  deriving (Show, Generic)

parseAnnotations :: Aeson.Value
                 -> Aeson.Parser ModuleAnnotations
parseAnnotations (Aeson.Object o) = do
  llvmPath <- o .: "llvm_path"
  binPath  <- o .: "binary_path"
  psize <- o .:! "page_size" .!= 4096
  guardCount <- o .:! "stack_guard_pages" .!= 1
  when (psize .&. (psize - 1) /= 0) $ do
    fail $ "Page size must be a power of 2."
  when (guardCount == 0) $ do
    fail $ "There must be at least one guard page."
  funs <- parseArray "functions" parseFunctionAnn o
  pure $! ModuleAnnotations { llvmFilePath = llvmPath
                            , binFilePath  = binPath
                            , pageSize     = psize
                            , stackGuardPageCount = guardCount
                            , functions    = funs
                            }
parseAnnotations _ =
  fail $ "Expected an object for the meta config."

instance Aeson.ToJSON ModuleAnnotations where
   toJSON ann =
     object [ "llvm_path"         .= llvmFilePath ann
            , "binary_path"       .= binFilePath ann
            , "page_size"         .= pageSize ann
            , "stack_guard_pages" .= stackGuardPageCount ann
            , "functions"         .= functions ann
            ]
