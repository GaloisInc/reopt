{-|
This defines a JSON format for providing hints to Reopt to help its
analysis.
-}
module Reopt.Hints
  ( FunIdent(..)
  , resolveSymName
--  , ReoptHints
--  , lookupFunType
  ) where

import           Data.Aeson as J
import qualified Data.ByteString.Char8 as BSC
import           Data.HashMap.Strict as H
import           Data.Hashable
--import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word
import Numeric

-- | A function identifier
data FunIdent
   = AddrIdent !Word64
   | SymbolIdent !BSC.ByteString
  deriving (Eq)


instance Hashable FunIdent where
  hashWithSalt i (AddrIdent w)   = hashWithSalt (hashWithSalt i (0::Int)) w
  hashWithSalt i (SymbolIdent w) = hashWithSalt (hashWithSalt i (1::Int)) w

-- | Resolve a hex string or other string as a address of symbol name.
resolveSymName :: String -> FunIdent
resolveSymName ('0':'x': nm) | [(w,"")] <- readHex nm = AddrIdent w
resolveSymName ('0':'X': nm) | [(w,"")] <- readHex nm = AddrIdent w
resolveSymName nm = SymbolIdent (BSC.pack nm)

{-
-- | This parses a subset of the LLVM types that can be used in Macaw.
--
-- Note. Currently, this only parses integers, but plan to extend this once
-- we know how.
parseLLVMType :: Text -> Either String (Some TypeRepr)
parseLLVMType t
  | Just r <- Text.stripPrefix "i" t
  , Right (v,"") <- Text.decimal r
  , Some n <- mkNatRepr v =
      case testLeq n1 n of
        Just LeqProof -> pure (Some (BVTypeRepr n))
        Nothing -> Left $ "i0 not allowed (i width must be positive)"
  | otherwise =
    Left $ "Could not recognize type: " ++ show t

type FunTypeMap = H.HashMap FunIdent FunType

-- | Hints to help reopt convert to LLVM.
data ReoptHints = ReoptHints { funTypeHints :: !(FunctionTypeMap X86_64) }

parseFunPair :: Value -> Parser (FunIdent, FunType)
parseFunPair v = do
  undefined v



parseFunPairMap' :: FunTypeMap
                 -> Int
                 -> V.Vector Value
                 -> Parser FunTypeMap
parseFunPairMap' m i v
  | i < V.length v = do
      (nm, tp) <- parseFunPair (v V.! i)
      when (H.member nm m) $ fail $ "Multiple type assignments to " ++ show nm
      let m' = H.insert nm tp m
      seq m' $ parseFunPairMap' m' (i+1) v
  | otherwise = do
      pure $! m

parseFunPairMap :: Value -> Parser FunTypeMap
parseFunPairMap =
  withArray "function_types" (parseFunPairMap' H.empty 0)

lookupFunType :: ReoptHints -> FunIdent -> Maybe FunType
lookupFunType hints nm = H.lookup (funTypeHints hints) nm

instance J.FromJSON ReoptHints where
  parseJSON = do
    withObject "hints" $ \o -> do
      m <- parseFunPairMap =<< o .: "function_types"
      pure $! ReoptHints { funTypeHints = m }
-}
