{-|
Declarations for function types pulled from user-provided header
file.
-}
module Reopt.AnnotatedTypes
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
   | CharAnnType
     -- ^ 8-bit signed integer.
   | ShortAnnType
     -- ^ A C short (on x86_64 is 16-bits)
   | IntAnnType
     -- ^ A C Int (on x86_64 is 32-bits)
   | LongAnnType
     -- ^ A C long (on x86_64 is 64-bits)
   | LongLongAnnType
     -- ^ A C long long (on x86_64 is 64-bits)
   | FloatAnnType
     -- ^ A C long long (on x86_64 is 64-bits)
   | DoubleAnnType
     -- ^ A C long long (on x86_64 is 64-bits)
   | BoolAnnType
     -- ^ A C _Bool (on x86_64 in LLVM is an i1)
   | PtrAnnType !AnnType
     -- ^ Pointer header type.
   | TypedefAnnType !BSC.ByteString !AnnType
     -- ^ A typedef with the name and resolved right hand side.
  deriving (Show)

-- | Pretty print the header type for the end user.
ppAnnType :: AnnType -> String
ppAnnType tp0 =
  case tp0 of
    VoidAnnType -> "void"
    CharAnnType -> "char"
    ShortAnnType -> "short"
    IntAnnType  -> "int"
    LongAnnType -> "long"
    LongLongAnnType -> "long long"
    FloatAnnType -> "float"
    DoubleAnnType -> "double"
    BoolAnnType -> "_Bool"
    PtrAnnType tp -> ppAnnType tp ++ "*"
    TypedefAnnType nm _ -> BSC.unpack nm

-- | Information about function argument with optional name
-- information.
data AnnFunArg = AnnFunArg { funArgName :: !(Maybe String)
                           , funArgType :: !AnnType
                           }
  deriving (Show)

-- | Types for a function declaration.
data AnnFunType = HdrFunType { funRet :: !AnnType
                             , funArgs :: !(V.Vector AnnFunArg)
                             , funVarArg :: !Bool
                             }
  deriving (Show)

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
