{-# LANGUAGE BlockArguments #-}

-- |
-- Type information pulled from user-provided header file.
module Reopt.TypeInference.Header (
  parseHeader,
) where

import Control.Monad (unless, when)
import Control.Monad.Except (Except, MonadError(throwError), runExcept)
import Control.Monad.State (gets, modify, StateT, execStateT)
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Language.C qualified as C

import Reopt.TypeInference.HeaderTypes

identToByteString :: C.Ident -> BSC.ByteString
identToByteString = BSC.pack . C.identToString

type CParser = StateT AnnDeclarations (Except (C.NodeInfo, String))

errorAt :: C.NodeInfo -> String -> CParser a
errorAt n s = throwError (n, s)

resolveTypedef :: C.NodeInfo -> C.Ident -> CParser AnnType
resolveTypedef n typeName = do
  m <- gets typeDefs
  let nm = identToByteString typeName
  case Map.lookup nm m of
    Nothing -> errorAt n $ "Unknown type " ++ BSC.unpack nm
    Just tp -> pure (TypedefAnnType nm tp)

data DeclLenMod
  = NoLenMod
  | ShortLenMod
  | LongLenMod
  | LongLongLenMod

-- | Integer type when an int is used.
declLenModIntType :: DeclLenMod -> AnnType
declLenModIntType dlm =
  case dlm of
    NoLenMod -> IAnnType 32
    ShortLenMod -> IAnnType 16
    LongLenMod -> IAnnType 64
    LongLongLenMod -> IAnnType 64

data QualMods = QM
  { qmLenMod :: !DeclLenMod
  , qmTypeSpec :: !(Maybe C.CTypeSpec)
  }

emptyQualMods :: QualMods
emptyQualMods =
  QM
    { qmLenMod = NoLenMod
    , qmTypeSpec = Nothing
    }

parseType :: QualMods -> C.CTypeSpec -> CParser AnnType
parseType qm tp =
  case tp of
    C.CVoidType _ -> pure VoidAnnType
    C.CCharType _ -> pure $! IAnnType 8
    C.CShortType _ -> pure $! IAnnType 16
    C.CIntType _ -> pure $! declLenModIntType (qmLenMod qm)
    C.CLongType _ -> pure $! IAnnType 64
    C.CFloatType _ -> pure FloatAnnType
    C.CDoubleType _ -> pure DoubleAnnType
    C.CSignedType _ -> pure $! IAnnType 32
    C.CUnsigType _ -> pure $! IAnnType 32
    C.CBoolType _ -> pure $! IAnnType 1
    C.CComplexType n -> errorAt n "_Complex is not supported."
    C.CInt128Type n -> errorAt n "__int128 is not supported."
    C.CUInt128Type n -> errorAt n "__uint128 is not supported."
    C.CFloatNType _ _ n -> errorAt n "Floating point extensions are not supported."
    C.CEnumType en _ -> parseEnum en
    C.CSUType su _ -> parseStructUnion su
    C.CTypeDef typeName n -> resolveTypedef n typeName
    C.CTypeOfExpr _ n -> errorAt n "typeof unsupported."
    C.CTypeOfType _ n -> errorAt n "typeof unsupported."
    C.CAtomicType _ n -> errorAt n "atomic is unsupported."

longShortError :: C.NodeInfo -> CParser a
longShortError n = errorAt n "Both 'long' and 'short in declaration specifier."

-- | Parse the declaration specifiers to get a header type.
parseQualType ::
  QualMods ->
  [C.CDeclarationSpecifier C.NodeInfo] ->
  CParser AnnType
-- We ignore qualifiers as they do not change layout.
parseQualType qm qtp =
  case qtp of
    C.CFunSpec _ : r -> parseQualType qm r
    C.CAlignSpec _ : r -> parseQualType qm r
    C.CTypeQual _ : r -> parseQualType qm r
    C.CStorageSpec _ : r -> parseQualType qm r
    C.CTypeSpec s : r ->
      case s of
        C.CVoidType _ -> pure VoidAnnType
        C.CCharType _ -> pure $! IAnnType 8
        C.CShortType n ->
          case qmLenMod qm of
            NoLenMod -> parseQualType qm{qmLenMod = ShortLenMod} r
            ShortLenMod -> errorAt n "Duplicate short"
            LongLenMod -> longShortError n
            LongLongLenMod -> longShortError n
        C.CIntType _ -> parseQualType qm r
        C.CLongType n ->
          case qmLenMod qm of
            NoLenMod -> parseQualType qm{qmLenMod = LongLenMod} r
            ShortLenMod -> longShortError n
            LongLenMod -> parseQualType qm{qmLenMod = LongLongLenMod} r
            LongLongLenMod -> errorAt n "Type is too long."
        C.CFloatType _ -> pure FloatAnnType
        C.CDoubleType _ -> pure DoubleAnnType
        C.CSignedType _ -> parseQualType qm r
        C.CUnsigType _ -> parseQualType qm r
        C.CBoolType _ -> pure $! IAnnType 1
        C.CComplexType n -> errorAt n "_Complex is not supported."
        C.CInt128Type n -> errorAt n "__int128 is not supported."
        C.CUInt128Type n -> errorAt n "__uint128 is not supported."
        C.CFloatNType _ _ n -> errorAt n "Floating point extensions are not supported."
        C.CEnumType en _ -> parseEnum en
        C.CSUType su _ -> parseStructUnion su
        C.CTypeDef typeName n -> resolveTypedef n typeName
        C.CTypeOfExpr _ n -> errorAt n "typeof unsupported."
        C.CTypeOfType _ n -> errorAt n "typeof unsupported."
        C.CAtomicType _ n -> errorAt n "atomic is unsupported."
    [] ->
      case qmTypeSpec qm of
        Nothing -> pure $! declLenModIntType (qmLenMod qm)
        Just ctp -> parseType qm ctp

parseEnum :: C.CEnum -> CParser AnnType
parseEnum (C.CEnum _mident _menumList _attrs n) =
  errorAt n "enum is not supported."

parseStructUnion :: C.CStructUnion -> CParser AnnType
parseStructUnion (C.CStruct tag _mi _mdecl _attrs n) =
  case tag of
    C.CStructTag -> errorAt n "Struct is not supported."
    C.CUnionTag -> errorAt n "Union is not supported."

parseTypeDecl :: C.NodeInfo -> C.CDeclarationSpecifier C.NodeInfo -> CParser C.CTypeSpec
parseTypeDecl _ (C.CTypeSpec spec) = return spec
parseTypeDecl n _ = errorAt n "Expected type specification"

parseTypeDecls :: C.CDeclaration C.NodeInfo -> CParser C.CTypeSpec
-- NOTE (val) So far I'm only seeing singleton lists here, not sure why.
parseTypeDecls (C.CDecl [decl] _ n) = parseTypeDecl n decl
parseTypeDecls (C.CDecl _decls _ n) =
  errorAt n "Expected single type spec, investigate."
parseTypeDecls (C.CStaticAssert _ _ n) =
  errorAt n "Expected declaration, found static assert, when parsing type declaration"

-- | Parse derived declarators.
parseTypeDerivedDecl :: [C.CDerivedDeclarator C.NodeInfo] -> AnnType -> CParser AnnType
parseTypeDerivedDecl [] tp = pure tp
parseTypeDerivedDecl (C.CPtrDeclr _ _ : rest) tp = do
  parseTypeDerivedDecl rest $! PtrAnnType tp
parseTypeDerivedDecl (C.CArrDeclr _ _ n : _) _tp = do
  errorAt n "Arrays are not supported."
parseTypeDerivedDecl (C.CFunDeclr (Right (decls, False)) _attrs _n : rest) tp = do
  typeSpecs <- mapM parseTypeDecls decls
  args <- mapM (parseType emptyQualMods) typeSpecs
  parseTypeDerivedDecl rest $! FunPtrAnnType tp args
  -- errorAt n $ "decls: " ++ show decls ++ "\nattrs: " ++ show attrs ++ "\ntp: " ++ show tp
parseTypeDerivedDecl (C.CFunDeclr _ _ n : _) _tp = do
  errorAt n "Function declarations are not supported in this context."

parseFullType ::
  [C.CDeclarationSpecifier C.NodeInfo] ->
  [C.CDerivedDeclarator C.NodeInfo] ->
  CParser AnnType
parseFullType tp derived = do
  parseTypeDerivedDecl derived =<< parseQualType emptyQualMods tp

parseFunDeclArg :: C.CDeclaration C.NodeInfo -> CParser AnnFunArg
parseFunDeclArg (C.CDecl ctype initDeclList n) = do
  tp <- parseQualType emptyQualMods ctype
  case initDeclList of
    -- Only type
    [] -> do
      pure $!
        AnnFunArg
          { funArgName = Nothing
          , funArgType = tp
          }
    [(Just declr, Nothing, Nothing)] -> do
      case declr of
        C.CDeclr mnm typeMod Nothing [] _ -> do
          qtp <- parseTypeDerivedDecl typeMod tp
          pure $!
            AnnFunArg
              { funArgName = C.identToString <$> mnm
              , funArgType = qtp
              }
        _ ->
          errorAt n $ "Unsupported declarator:\n" ++ show declr ++ "\n"
    _ -> do
      errorAt n $
        "Invalid declaration list:\n"
          ++ show initDeclList
          ++ "\n"
parseFunDeclArg (C.CStaticAssert _ _ n) =
  errorAt n "Unexpected static assertion inside argument."

-- | Parse declarations
parseExtDeclaration :: C.CExternalDeclaration C.NodeInfo -> CParser ()
parseExtDeclaration (C.CDeclExt d) = do
  case d of
    -- Parse typedef
    C.CDecl
      (C.CStorageSpec (C.CTypedef _) : ctype)
      [(Just (C.CDeclr (Just typeIdent) typeMod Nothing [] _), Nothing, Nothing)]
      _n -> do
        m <- gets typeDefs
        let nm = identToByteString typeIdent
        when (Map.member nm m) $
          errorAt (C.nodeInfo typeIdent) $
            BSC.unpack nm ++ " already defines a type."
        tp <- parseFullType ctype typeMod
        modify $ \s -> s{typeDefs = Map.insert nm tp (typeDefs s)}
    C.CDecl [C.CTypeSpec ctype] [(Just declr, Nothing, Nothing)] n -> do
      case declr of
        C.CDeclr (Just fnIdent) (C.CFunDeclr cparams attrs _ : derived) Nothing [] _ -> do
          unless (null attrs) $ do
            errorAt n "Functions may not have attributes."
          case cparams of
            Left _ -> errorAt n "Function declarations require explicit arguments."
            Right (cArgs, varArgs) -> do
              args <- traverse parseFunDeclArg (V.fromList cArgs)
              retType <- parseTypeDerivedDecl derived =<< parseType emptyQualMods ctype
              when varArgs $ errorAt n "Vararg functions unsupported."
              let fd =
                    AnnFunType
                      { funRet = retType
                      , funArgs = args
                      }
              m <- gets funDecls
              let nm = BSC.pack (C.identToString fnIdent)
              when (Map.member nm m) $ do
                errorAt n $ "Function " ++ C.identToString fnIdent ++ " already defined."
              modify $ \s -> s{funDecls = Map.insert nm fd (funDecls s)}
        C.CDeclr mname indirections masmName attrs declNode -> do
          errorAt n $
            "Unexpected external declarator:\n"
              ++ show mname
              ++ "\n"
              ++ show indirections
              ++ "\n"
              ++ show masmName
              ++ "\n"
              ++ show attrs
              ++ "\n"
              ++ show declNode
              ++ "\n"
    C.CDecl [C.CTypeSpec _] (_ : _ : _) n -> errorAt n "More than one thing in the list"
    C.CDecl [C.CTypeSpec _] [_] n -> errorAt n "Not (J, N, N)"
    C.CDecl specs bleh n -> do
      errorAt n $
        "Unexpected declaration:\n"
          ++ show specs
          ++ "\n"
          ++ show bleh
          ++ "\n"
          ++ show n
    C.CStaticAssert _ _ n -> do
      errorAt n "Static assertions not supported."
parseExtDeclaration (C.CFDefExt d) = do
  errorAt (C.annotation d) "Function definitions are not supported."
parseExtDeclaration (C.CAsmExt _ n) = do
  errorAt n "Assembly is not supported."

-- | Parse a translation unit.
parseTransUnit :: C.CTranslUnit -> Either (C.NodeInfo, String) AnnDeclarations
parseTransUnit (C.CTranslUnit decls _r) =
  let e = emptyAnnDeclarations
   in runExcept (execStateT (traverse_ parseExtDeclaration decls) e)

-- | This parses a C file that effectively provides hints to the
-- decompiler.
parseHeader ::
  -- | Path to header clang
  FilePath ->
  BSC.ByteString ->
  Either String AnnDeclarations
parseHeader fname inputStream = do
  -- Run Language.C parser
  ctu <-
    case C.parseC inputStream (C.initPos fname) of
      Left e -> do
        Left $
          unlines
            [ "Language.C could not parse header " ++ fname
            , "  " ++ show e
            ]
      Right ctu ->
        pure ctu
  -- Parse the compiled code.
  case parseTransUnit ctu of
    Left (_, e) -> do
      Left $
        unlines $
          ("Error parsing header in " ++ fname)
            : ["  " ++ m | m <- lines e]
    Right r -> do
      pure $! r
