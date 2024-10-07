{-# LANGUAGE OverloadedStrings #-}

-- | Declarations for function types pulled from user-provided header file.
module Reopt.TypeInference.HeaderTypes (
  AnnDeclarations (..),
  emptyAnnDeclarations,
  AnnFunType (..),
  AnnFunArg (..),
  AnnType (..),
  ppAnnType,
  isFnPtr,
) where

import Data.ByteString.Char8 qualified as BSC
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Prettyprinter qualified as PP

-- | Types supported by annotations.
--
-- This needs to be both something other code can be parse, and
-- capable of being rendered to the user.
data AnnType
  = -- | Void type (not directly allowed in arguments)
    VoidAnnType
  | -- | An integer type with the given number of bits.
    IAnnType !Int
  | -- | A C float (32bits IEEE)
    FloatAnnType
  | -- | A C double (64bits IEEE)
    DoubleAnnType
  | -- | Pointer header type.
    PtrAnnType !AnnType
  | -- | A typedef with the name and resolved right hand side.
    TypedefAnnType !BSC.ByteString !AnnType
  | FunPtrAnnType !AnnType ![AnnType]
  deriving (Eq, Show, Read)

isFnPtr :: AnnType -> Bool
isFnPtr FunPtrAnnType{} = True
isFnPtr _ = False

-- | Pretty print the header type for the end user.
ppAnnType :: AnnType -> String
ppAnnType = \case
  VoidAnnType -> "void"
  IAnnType w -> "i" <> show w
  FloatAnnType -> "float"
  DoubleAnnType -> "double"
  PtrAnnType tp -> ppAnnType tp ++ "*"
  TypedefAnnType nm _ -> BSC.unpack nm
  FunPtrAnnType ret args -> ppAnnType ret ++ "(*?)(" ++ intercalate ", " (map ppAnnType args) ++ ")"

instance PP.Pretty AnnType where
  pretty = \case
    VoidAnnType -> "void"
    IAnnType w -> "i" <> PP.pretty w
    FloatAnnType -> "float"
    DoubleAnnType -> "double"
    PtrAnnType tp -> PP.pretty tp <> "*"
    TypedefAnnType nm _ -> PP.pretty (BSC.unpack nm)
    FunPtrAnnType ret args ->
      PP.pretty ret PP.<+> PP.parens (PP.hcat (PP.punctuate PP.comma (map PP.pretty args)))

-- | Information about function argument with optional name
-- information.
data AnnFunArg = AnnFunArg
  { funArgName :: !(Maybe String)
  , funArgType :: !AnnType
  }
  deriving (Eq, Show, Read)

instance PP.Pretty AnnFunArg where
  pretty arg =
    case funArgName arg of
      Nothing -> PP.pretty (funArgType arg)
      Just nm -> PP.hsep [PP.pretty nm, ":", PP.pretty (funArgType arg)]

-- | Types for a function declaration.
data AnnFunType = AnnFunType
  { funRet :: !AnnType
  , funArgs :: !(V.Vector AnnFunArg)
  }
  deriving (Eq, Show, Read)

instance PP.Pretty AnnFunType where
  pretty fty =
    PP.hcat
      [ PP.pretty (funRet fty)
      , PP.space
      , "("
      , PP.hcat (PP.punctuate (PP.comma <> PP.space) (map PP.pretty (V.toList (funArgs fty))))
      , ")"
      ]

-- | Annotations provided by the user in the form of a header file.
data AnnDeclarations = AnnDeclarations
  { typeDefs :: !(Map BSC.ByteString AnnType)
  , funDecls :: !(Map BSC.ByteString AnnFunType)
  -- ^ Map from function name to type.
  }
  deriving (Show)

-- | Empty header
emptyAnnDeclarations :: AnnDeclarations
emptyAnnDeclarations =
  AnnDeclarations
    { typeDefs = Map.empty
    , funDecls = Map.empty
    }
