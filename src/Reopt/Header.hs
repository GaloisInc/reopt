{-|
Function information pulled from user-provided header file.
-}
module Reopt.Header
  ( Header(..)
  , emptyHeader
  , parseHeader
  , HdrFunDecl(..)
  , HdrFunArg(..)
  , HdrType(..)
  , ppHdrType
  ) where


import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import           System.Exit
import           System.IO
import qualified Language.C as C

import Reopt.ExternalTools (runClangPreprocessor)

-- | Types supported by annotations.
--
-- This needs to be both something other code can be parse, and
-- capable of being rendered to the user.
data HdrType
   = CharHdrType
     -- ^ 8-bit signed integer.
   | UInt64HdrType
     -- ^ 64-bit unsigned integer.
   | VoidHdrType
     -- ^ Void type (not directly allowed in arguments)
   | PtrHdrType !HdrType
     -- ^ Pointer header type.
   | TypedefHdrType !String !HdrType
     -- ^ A typedef with the name and resolved right hand side.
  deriving (Show)

-- | Pretty print the header type for the end user.
ppHdrType :: HdrType -> String
ppHdrType tp0 =
  case tp0 of
    CharHdrType -> "char"
    UInt64HdrType -> "long"
    VoidHdrType -> "void"
    PtrHdrType tp -> "*" ++ ppHdrType tp
    TypedefHdrType nm _ -> nm

-- | Information about function argument with optional name
-- information.
data HdrFunArg = HdrFunArg { funArgName :: !(Maybe String)
                           , funArgType :: !HdrType
                           }
  deriving (Show)

-- | Types for a function declaration.
data HdrFunDecl = HdrFunDecl { hfdRet :: !HdrType
                             , hfdArgs :: !(V.Vector HdrFunArg)
                             , hfdVarArg :: !Bool
                             }
  deriving (Show)


-- | Annotations provided by the user in the form of a header file.
data Header
   = Header { hdrTypeDefs :: !(Map String HdrType)
            , hdrFunDecls :: !(Map BSC.ByteString HdrFunDecl)
            }
  deriving (Show)

-- | Empty header
emptyHeader :: Header
emptyHeader = Header { hdrTypeDefs = Map.empty
                     , hdrFunDecls = Map.empty
                     }


type CParser = StateT Header (Except (C.NodeInfo, String))

errorAt :: C.NodeInfo -> String -> CParser a
errorAt n s = throwError (n,s)

parseType :: C.CTypeSpec -> CParser HdrType
parseType (C.CCharType _)  = pure $! CharHdrType
parseType (C.CLongType _) = pure $! UInt64HdrType
parseType (C.CVoidType _) = pure $! VoidHdrType
parseType (C.CTypeDef typeName n) = do
  m <- gets $ hdrTypeDefs
  let nm = C.identToString typeName
  case Map.lookup nm m of
    Nothing -> errorAt n $ "Unknown type " ++ nm
    Just tp -> pure (TypedefHdrType nm tp)
parseType t = errorAt (C.annotation t) $ "Unexpected type " ++ show t

parseQualType :: [C.CDeclarationSpecifier C.NodeInfo] -> CParser HdrType
-- We ignore qualifiers as they do not change layout.
parseQualType (C.CTypeQual _ : r) = parseQualType r
parseQualType [C.CTypeSpec ctype] = parseType ctype
parseQualType (a:_) =
  errorAt (C.annotation a) $ "Unexpected entry in qualified type " ++ show a
parseQualType [] =
  error "Qualified type is empty"

-- | Parser derived declarators.
parseTypeDerivedDecl :: [C.CDerivedDeclarator C.NodeInfo] -> HdrType -> CParser HdrType
parseTypeDerivedDecl [] tp = pure tp
parseTypeDerivedDecl (C.CPtrDeclr _ _:rest) tp = do
  parseTypeDerivedDecl rest $! PtrHdrType tp
parseTypeDerivedDecl (C.CArrDeclr _ _ n:_) _tp = do
  errorAt n $ "Arrays are not supported."
parseTypeDerivedDecl (C.CFunDeclr _ _ n:_) _tp = do
  errorAt n $ "Function declarations are not supported in this context."

parseFullType :: [C.CDeclarationSpecifier C.NodeInfo]
              -> [C.CDerivedDeclarator C.NodeInfo]
              -> CParser HdrType
parseFullType tp derived = do
  parseTypeDerivedDecl derived =<< parseQualType tp


parseFunDeclArg :: C.CDeclaration C.NodeInfo -> CParser HdrFunArg
parseFunDeclArg (C.CDecl ctype [(Just (C.CDeclr mnm typeMod Nothing [] _), Nothing, Nothing)] _n) = do
  tp <- parseFullType ctype typeMod
  pure $! HdrFunArg { funArgName = C.identToString <$> mnm
                    , funArgType = tp
                    }
parseFunDeclArg (C.CDecl ctype [] _n) = do
  tp <- parseQualType ctype
  pure $! HdrFunArg { funArgName = Nothing
                    , funArgType = tp
                    }
parseFunDeclArg (C.CDecl specs bleh n) =
  errorAt n $ "Unexpected declaration:\n"
                 ++ show specs ++ "\n"
                 ++ show bleh ++ "\n"
                 ++ show n
parseFunDeclArg (C.CStaticAssert _ _ n) =
  errorAt n $ "Unexpected static assertion inside argument."

-- | Parse declarations
parseExtDeclaration :: C.CExternalDeclaration C.NodeInfo -> CParser ()
parseExtDeclaration (C.CDeclExt d) = do
  case d of
    C.CDecl (C.CStorageSpec (C.CTypedef _) : ctype)
            [(Just (C.CDeclr (Just typeIdent) typeMod Nothing [] _), Nothing, Nothing)]
            _n -> do
      m <- gets hdrTypeDefs
      let nm = C.identToString typeIdent
      when (Map.member nm m) $
        errorAt (C.nodeInfo typeIdent) $ nm ++ " already defines a type."
      tp <- parseFullType ctype typeMod
      modify $ \s -> s { hdrTypeDefs = Map.insert nm tp (hdrTypeDefs s) }
    C.CDecl [C.CTypeSpec ctype]
            [(Just (C.CDeclr (Just fnIdent) [C.CFunDeclr cparams attrs _] Nothing [] _), Nothing, Nothing)]
            n -> do
      when (not (null attrs)) $ do
        errorAt n $ "Functions may not have attributes."
      case cparams of
          Left _ -> errorAt n "Function delcarions require explicit arguments."
          Right (cArgs, varArgs) -> do
            args <- traverse parseFunDeclArg (V.fromList cArgs)
            retType <- parseType ctype
            let fd = HdrFunDecl { hfdRet = retType
                                , hfdArgs = args
                                , hfdVarArg = varArgs
                                }
            m <- gets hdrFunDecls
            let nm = BSC.pack (C.identToString fnIdent)
            when (Map.member nm m) $ do
              errorAt n $ "Function " ++ C.identToString fnIdent ++ " already defined."
            modify $ \s -> s { hdrFunDecls = Map.insert nm fd (hdrFunDecls s) }

    C.CDecl specs bleh n -> do
      errorAt n $ "Unexpected declaration:\n"
                 ++ show specs ++ "\n"
                 ++ show bleh ++ "\n"
                 ++ show n
    C.CStaticAssert _ _ n -> do
      errorAt n $ "Static assertions not supported."
parseExtDeclaration (C.CFDefExt d) = do
  errorAt (C.annotation d)  "Function definitions are not supported."
parseExtDeclaration (C.CAsmExt _ n) = do
  errorAt n "Assembly is not supported."

-- | Parse a translation unit.
parseTransUnit :: C.CTranslUnit -> Either (C.NodeInfo, String) Header
parseTransUnit (C.CTranslUnit decls _r) =
  let e = emptyHeader
   in runExcept (execStateT (traverse_ parseExtDeclaration decls) e)

-- | This parses a C file that effectively provides hints to the
-- decompiler.
--
-- It writes errors to stderr and exits the program if parsing fails.
parseHeader :: FilePath -- ^ Path to clang
            -> FilePath -- ^ Path to header clang
            -> IO Header
parseHeader clangCmd fname = do
  inputStream <- do
    maybeInputStream <- runExceptT $ runClangPreprocessor clangCmd fname
    case maybeInputStream of
      Left e -> do
        hPutStrLn stderr (show e)
        exitFailure
      Right s -> pure s
  -- Run Language.C parser
  ctu <-
    case C.parseC inputStream (C.initPos fname) of
      Left e -> do
        hPutStrLn stderr $ "Language.C could not parse header " ++ fname
        hPutStrLn stderr $ "  " ++ show e
        exitFailure
      Right ctu ->
        pure ctu
  -- Parse the compiled code.
  case parseTransUnit ctu of
    Left (_,e) -> do
      hPutStrLn stderr $ "Error parsing header in " ++ fname
      forM_ (lines e) $ \m -> do
        hPutStrLn stderr $ "  " ++ m
      exitFailure
    Right r -> do
      pure $! r
