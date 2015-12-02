{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Main where

import           Control.Lens
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Either (either)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, maybe)
import           Language.C
import           Language.C.Analysis.AstAnalysis
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TravMonad
import           Language.C.Data.Ident
import           Language.C.Data.Name (newNameSupply)
import           Language.C.Data.Position (position)
import           Text.PrettyPrint
import           Text.Show.Pretty

import           Debug.Trace

-- FIXME: clag from Reopt.Machine.SysDeps.Types
data SyscallArgType = VoidArgType | WordArgType | XMMFloatType
                    deriving (Eq, Show, Read)


-- | The syscall.master file looks like (after pre-processing)
--
-- # <line no> filename
-- ... c header stuff
-- # <line no> "syscalls.master"
-- ; comment line
-- ...
-- <syscall no.>\t<audit>\t<syscall type>\t<prototype> ... ignored stuff ...
--
-- for example
--
-- 538 43207 STD { int bindat(int fd, int s, caddr_t name, int namelen); }
--
-- This file then does the following:
-- 1. Divide the file into two parts: the C headers and the syscall defs
-- 2. Parse each line in the syscall defs to extract the propotype
-- 3. Extract the argument types from the C prototype.

data SyscallInfo =
  SyscallInfo { syscallNo    :: Integer
              , syscallAudit :: Integer
              , syscallType  :: ByteString
              , syscallProto :: Maybe CExtDecl
              }
  deriving Show

pp si = print $ integer (syscallNo si) <+> text "->"
                <+> maybe (text "???") pretty (syscallProto si)

-- A bit hacky, but no less than using TH to read in a text file
generateHSFile :: CTranslUnit -> [SyscallInfo] -> Doc
generateHSFile tunit sis =
  vcat [ text "-- DO NOT EDIT.  Generated from make_bsd_syscalls/Main.hs"
       , text "module Reopt.Machine.SysDeps.FreeBSDGenerated where"
       , text "import           Reopt.Machine.SysDeps.Types"
       , text "import           Data.Map (Map, fromList)"
       , text "import           Data.Word"         
       , text ""
       , text "syscallInfo :: Map Word64 SyscallTypeInfo"
       , text "syscallInfo =" <+> ppDoc syscallMap ]
  where
    syscallMap = Map.fromList [ (syscallNo si, info)
                              | si <- sis
                              , Just d <- [ syscallProto si ]
                              , Right (info, _) <- [ syscallInfo d ] ]
    -- syscallInfo :: CExtDecl -> Maybe (String, SyscallArgType, [SyscallArgType])
    syscallInfo cdecl =
      runTrav (error "no decl") $ do
        _ <- analyseAST tunit
        let handler (DeclEvent idecl) = modifyUserState (const $ summariseDeclEvent idecl)
            handler _                 = return ()
        withExtDeclHandler (analyseExt cdecl) handler
        getUserState

    -- summariseDeclEvent idecl = pretty idecl
    
    summariseDeclEvent (getVarDecl -> VarDecl vname _ (FunctionType (FunType rettyp params _) _)) =
      ( identToString (identOfVarName vname)
      , typeToArgType rettyp
      , map (typeToArgType . declType) params )

    -- syscallInfo (CDeclExt (CDecl [CTypeSpec spec] [(Just declr, _, _)] _))
    --   | CDeclr (Just ident) [CFunDeclr (Right (decls, _)) _ _] _ _ _ <- declr
    --   = Just (identToString ident, typeSpecToArgType spec, map declToArgType decls)              
    -- syscallInfo d = error ("unhandled decl" ++ show d)
    
    -- declToArgType (CDecl [CTypeSpec spec]  _) = typeSpecToArgType spec
    -- declToArgType d = error ("unhandled decl (in type) " ++ show d)

typeToArgType :: Type -> SyscallArgType
typeToArgType typ =
  case typ of
    DirectType typ' _ _ ->
      case typ' of 
        TyVoid               -> VoidArgType
        TyIntegral _         -> WordArgType
        TyFloating TyLDouble -> unhandled
        TyFloating _         -> XMMFloatType
        TyComplex _          -> unhandled
        TyComp comp          -> unhandled -- compTypeToArgType comp
        TyEnum _             -> WordArgType -- FIXME: ???
        TyBuiltin _          -> unhandled
    PtrType _ _ _            -> WordArgType
    ArrayType _ _ _ _        -> WordArgType
    FunctionType _ _         -> unhandled
    TypeDefType (TypeDefRef _ (Just typ) _) _ _ -> typeToArgType typ
  where
    unhandled = error ("Unhandled type: " ++ show (pretty typ))

compTypeToArgType :: CompTypeRef -> SyscallArgType
compTypeToArgType ctyp = trace ("Comp type: " ++ show (pretty ctyp)) WordArgType

-- We support as little as possible here ...
-- typeSpecToArgType :: Show a => CTypeSpecifier a -> SyscallArgType
-- typeSpecToArgType tspec =
--   case tspec of
--     CVoidType _   -> VoidArgType
--     CCharType _   -> WordArgType
--     CShortType _  -> WordArgType
--     CIntType _    -> WordArgType
--     CLongType _   -> WordArgType
--     CFloatType _  -> XMMFloatType
--     CDoubleType _ -> XMMFloatType 
--     CSignedType _ -> WordArgType
--     CUnsigType _  -> WordArgType
--     CBoolType _   -> WordArgType
--     CSUType _ _ -> unhandled
--     CEnumType _ _ -> WordArgType -- FIXME: does it fit in a word?
--     CComplexType _ -> unhandled
--     CTypeDef __dent _ -> unhandled
--     CTypeOfExpr _ _ -> unhandled
--     CTypeOfType _ _ -> unhandled
--   where
--     unhandled = error ("Unhandled type specifier: " ++ show tspec)



main = do
  ls <- BS.split '\n' <$> BS.getContents

  let (split_headers, syscalls) = splitFile (filter (not . BS.null) ls)
      headers = BS.intercalate "\n" split_headers
      Right tunit  = parseC headers (position 0 "" 0 0)
      idents       = translUnitToIdents tunit
  ms <- case mapM (P.parseOnly (syscallLine idents)) (tail syscalls) of
          Left err -> error (show err)
          Right ms -> return ms

  -- mapM_ pp (catMaybes ms)
  print (generateHSFile tunit $ catMaybes ms)

translUnitToIdents :: CTranslUnit -> [Ident]
translUnitToIdents (CTranslUnit decls _) =
  [ ident | CDeclExt (CDecl _ tdecls _) <- decls
          , (Just (CDeclr (Just ident) _ _ _ _), _, _) <- tdecls ]

splitFile :: [InputStream] -> ([InputStream], [InputStream])
splitFile = go False mempty
  where
    go :: Bool -> ([InputStream], [InputStream]) -> [InputStream] -> ([InputStream], [InputStream])
    go _ acc [] = acc
    go inSyscalls acc (l : ls)
      | Just (_, filename) <- isCPPLinePragma l
        = go (filename == "syscalls.master") acc ls
      | otherwise = go inSyscalls (acc & (if inSyscalls then _2 else _1) %~ (++ [l])) ls

isCPPLinePragma :: InputStream -> Maybe (Integer, String)
isCPPLinePragma str =
  case P.parseOnly cppLinePragma str of
    Left _  -> Nothing
    Right r -> Just r

cppLinePragma :: Parser (Integer, String)
cppLinePragma = do
  _ <- P.string "# "
  n <- P.decimal
  _ <- P.space
  _ <- P.char '"'
  filename <- P.many1 (P.satisfy ((/=) '"'))
  _ <- P.char '"'
  -- .. other stuff until end of line, we don't really care though
  return (n, filename)

syscallLine :: [Ident] -> Parser (Maybe SyscallInfo)
syscallLine idents =
  P.choice [ P.char ';' >> return Nothing
           , parseLine
           ]
  where
    parseLine = do
      num   <- P.decimal
      P.skipSpace
      audit <- P.decimal
      P.skipSpace       
      cl    <- P.takeWhile (not . P.isSpace)
      P.skipSpace
      cdecl <- P.choice [ P.char '{' >> P.takeTill ((==) '}') >>= return . parseDecl
                        , P.takeWhile1 (not . P.isSpace)      >>  return Nothing
                        ]
      return (Just (SyscallInfo num audit cl cdecl))

    parseDecl bytes =
      -- FIXME: we should maybe chain through newNameSupply?  I don't think it is ever used ...
      case execParser extDeclP bytes (position 0 "" 0 0) idents newNameSupply of
        Left _err                   -> Nothing
        Right (cdecl, _unusedNames) -> Just cdecl
