{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Main
  ( main
  ) where

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
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           Text.PrettyPrint
import           Text.Show.Pretty

import           Debug.Trace

-- FIXME: clag from Data.Macaw.Architecture.Syscall
data SyscallArgType = VoidArgType | WordArgType
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

------------------------------------------------------------------------
-- Haskell generation

-- A bit hacky, but no less than using TH to read in a text file
generateHSFile :: CTranslUnit -> [SyscallInfo] -> Doc
generateHSFile tunit sis =
  vcat [ text "-- DO NOT EDIT.  Generated from make_bsd_syscalls/Main.hs"
       , text "module Data.Macaw.X86.FreeBSDGenerated where"
       , text "import           Data.Macaw.Architecture.Syscall"
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
        TyFloating _         -> unhandled -- XMMFloatType
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

------------------------------------------------------------------------
-- File preprocessing

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

-- | This attempts to parses a bytestring line of the form:
--
--  "# ?decimal "?filename".*
--
-- If it matches this, then it returns the decimal value and
-- filenumber.  If it does not, then it returns nothing.
isCPPLinePragma :: BS.ByteString
                -> Maybe (Integer, String)
isCPPLinePragma str =
  case P.parseOnly cppLinePragma str of
    Left _  -> Nothing
    Right r -> Just r

-- | This takes the lines in a file, and returns a pair of lines.
-- The first contains those lines between a pragma of the form
--
--   # XX  "syscalls.master"
--
-- and a pragma with any other filename.
--
-- The second contains the other lines.
splitFile :: [BS.ByteString] -> ([BS.ByteString], [BS.ByteString])
splitFile = go False mempty
  where
    go :: Bool
          -- ^ This flag is true if the last CPP line pragma had the filemname
          -- syscalls.master"
       -> ([BS.ByteString], [BS.ByteString])
          -- ^ This contains the lines outside the system call
          -- contents and the lines inside of respectively.
       -> [BS.ByteString]
       -> ([BS.ByteString], [BS.ByteString])
    go _ acc [] = acc
    go inSyscalls acc (l : ls)
      | Just (_, filename) <- isCPPLinePragma l
        = go (filename == "syscalls.master") acc ls
        -- Add lines in the system
      | otherwise =
        go inSyscalls (acc & (if inSyscalls then _1 else _2) %~ (++ [l])) ls

------------------------------------------------------------------------
-- Parsing

translUnitToIdents :: CTranslUnit -> [Ident]
translUnitToIdents (CTranslUnit decls _) =
  [ ident | CDeclExt (CDecl _ tdecls _) <- decls
          , (Just (CDeclr (Just ident) _ _ _ _), _, _) <- tdecls ]

------------------------------------------------------------------------
-- Main

parseSyscallLine :: [Ident] -> BS.ByteString -> IO (Maybe SyscallInfo)
parseSyscallLine idents l =
  case P.parseOnly (syscallLine idents) l of
    Left err -> do
      hPutStrLn stderr $ "Could not parse system call:"
      hPutStrLn stderr $ "  " ++ show err
      hPutStrLn stderr $ "  Input: " ++ BS.unpack l
      exitFailure
    Right i ->
      return i

showUsageAndExit :: IO a
showUsageAndExit = do
  hPutStrLn stderr $ unlines
    [ "This program generates the Haskell module that maps system call ids"
    , "in FreeBSD to the name, argument types, and result type."
    , ""
    , "Please specify the system.master.cpp file as the first argument."
    , "The resulting Haskell module will be written to standard out."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  input <-
    case args of
      [input] -> pure input
      _ -> showUsageAndExit

      -- Get contents and split into lines.
  ls <- BS.split '\n' <$> BS.readFile input

  let (syscalls, split_headers) = splitFile (filter (not . BS.null) ls)
      headers = BS.intercalate "\n" split_headers
      Right tunit  = parseC headers (position 0 "" 0 0)
      idents       = translUnitToIdents tunit

  ms <- mapM (parseSyscallLine idents) (tail syscalls)
  putStrLn "Got ms"
  -- mapM_ pp (catMaybes ms)
  print $ generateHSFile tunit $ catMaybes ms
