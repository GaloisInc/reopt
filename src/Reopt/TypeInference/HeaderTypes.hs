{-|
Declarations for function types pulled from user-provided header file.
-}
module Reopt.TypeInference.HeaderTypes
  ( AnnDeclarations(..)
  , emptyAnnDeclarations
  , AnnFunType(..)
  , AnnFunArg(..)
  , AnnType(..)
  , ppAnnType
  ) where

import qualified Data.ByteString.Char8 as BSC
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- | Types supported by annotations.
--
-- This needs to be both something other code can be parse, and
-- capable of being rendered to the user.
data AnnType
   = VoidAnnType
     -- ^ Void type (not directly allowed in arguments)
   | IAnnType !Int
     -- ^ An integer type with the given number of bits.
   | FloatAnnType
     -- ^ A C float (32bits IEEE)
   | DoubleAnnType
     -- ^ A C double (64bits IEEE)
   | PtrAnnType !AnnType
     -- ^ Pointer header type.
   | TypedefAnnType !BSC.ByteString !AnnType
     -- ^ A typedef with the name and resolved right hand side.
  deriving (Eq, Show, Read)

-- | Pretty print the header type for the end user.
ppAnnType :: AnnType -> String
ppAnnType tp0 =
  case tp0 of
    VoidAnnType -> "void"
    IAnnType w  -> "i" <> show w
    FloatAnnType -> "float"
    DoubleAnnType -> "double"
    PtrAnnType tp -> ppAnnType tp ++ "*"
    TypedefAnnType nm _ -> BSC.unpack nm

-- | Information about function argument with optional name
-- information.
data AnnFunArg = AnnFunArg { funArgName :: !(Maybe String)
                           , funArgType :: !AnnType
                           }
  deriving (Eq, Show, Read)

-- | Types for a function declaration.
data AnnFunType = AnnFunType { funRet :: !AnnType
                             , funArgs :: !(V.Vector AnnFunArg)
                             }
  deriving (Eq, Show, Read)

-- | Annotations provided by the user in the form of a header file.
data AnnDeclarations
   = AnnDeclarations { typeDefs :: !(Map BSC.ByteString AnnType)
                     , funDecls :: !(Map BSC.ByteString AnnFunType)
                     -- ^ Map from function name to type.
                     }
  deriving (Show)

-- | Empty header
emptyAnnDeclarations :: AnnDeclarations
emptyAnnDeclarations = AnnDeclarations { typeDefs = Map.empty
                                       , funDecls = Map.empty
                                       }
