{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.DebugTypes (
  ResolveAddrFn,
  resolveDebugFunTypes,
) where

import Control.Monad (
  unless,
  when,
 )
import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  runExceptT,
 )
import Control.Monad.Extra (forM)
import Control.Monad.Trans (
  MonadTrans (lift),
 )
import Data.ByteString.Char8 qualified as BSC
import Data.ElfEdit qualified as Elf
import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Vector qualified as V
import Data.Word (Word64)

import Prettyprinter (pretty)
import Text.Printf (printf)

import Data.Macaw.CFG (
  ArchAddrWidth,
  ArchSegmentOff,
  MemSegmentOff,
  MemWidth,
 )
import Data.Macaw.Discovery (
  NoReturnFunStatus (MayReturnFun, NoReturnFun),
 )
import Data.Macaw.Dwarf qualified as Dwarf
import Data.Macaw.Utils.IncComp (IncCompM, incCompLog)

import Reopt.ArgResolver (
  ArgResolverError (
    DebugResolveError,
    MissingArgType,
    VarArgsUnsupported
  ),
  showArgResolverError,
 )
import Reopt.Events (
  ReoptGlobalStep (DebugTypeInference),
  ReoptLogEvent (
    ReoptGlobalStepFinished,
    ReoptGlobalStepStarted,
    ReoptGlobalStepWarning
  ),
 )
import Reopt.TypeInference.FunTypeMaps (
  FunTypeMaps (noreturnMap),
  ReoptFunType (ReoptNonvarargFunType),
  addNamedFunType,
  funTypeIsDefined,
 )
import Reopt.TypeInference.HeaderTypes (
  AnnFunArg (..),
  AnnFunType (..),
  AnnType (
    DoubleAnnType,
    FloatAnnType,
    FunPtrAnnType,
    IAnnType,
    PtrAnnType,
    VoidAnnType
  ),
 )

reoptTypeWarning :: String -> IncCompM (ReoptLogEvent arch) r ()
reoptTypeWarning msg =
  incCompLog $
    ReoptGlobalStepWarning DebugTypeInference msg

-- | Get name as an external symbol
dwarfExternalName :: Dwarf.Subprogram -> Maybe BSC.ByteString
dwarfExternalName sub
  | Dwarf.subExternal sub, Dwarf.subName sub /= "" = Just $ Dwarf.nameVal $ Dwarf.subName sub
  | otherwise = Nothing

dwarfSubEntry :: Dwarf.Subprogram -> Maybe Word64
dwarfSubEntry sub =
  case Dwarf.subEntryPC sub of
    Just e -> Just e
    Nothing -> Dwarf.subLowPC =<< Dwarf.subDef sub

throwDwarfTypeError :: MonadError ArgResolverError m => Dwarf.TypeRef -> String -> m a
throwDwarfTypeError ref msg =
  throwError $ DebugResolveError $ printf "Bad type ref 0x%x: %s" (Dwarf.typeRefFileOffset ref) msg

resolveDwarfTypeRef ::
  -- | Logging function for errors
  Map Dwarf.TypeRef Dwarf.AbsType ->
  Dwarf.TypeRef ->
  ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) Dwarf.TypeApp
resolveDwarfTypeRef typeMap ref = do
  case Map.lookup ref typeMap of
    Nothing -> do
      let o = Dwarf.typeRefFileOffset ref
      throwDwarfTypeError ref $ printf "Could not find type %x." o
    Just (Left msg, warnings) -> do
      lift $ mapM_ reoptTypeWarning warnings
      throwDwarfTypeError ref $ printf "Dwarf parsing error: %s" msg
    Just (Right tp, warnings) -> do
      lift $ mapM_ reoptTypeWarning warnings
      pure tp

resolveDwarfType ::
  Map Dwarf.TypeRef Dwarf.AbsType ->
  Dwarf.TypeRef ->
  ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) AnnType
resolveDwarfType typeMap ref = do
  tp <- resolveDwarfTypeRef typeMap ref
  resolveDwarfTypeApp typeMap ref tp

resolveDwarfTypeApp ::
  Map Dwarf.TypeRef Dwarf.AbsType ->
  -- | Still needed for reporting errors
  Dwarf.TypeRef ->
  Dwarf.TypeApp ->
  ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) AnnType
resolveDwarfTypeApp typeMap ref tApp =
  case tApp of
    Dwarf.BoolType -> do
      pure $! IAnnType 1
    Dwarf.UnsignedIntType byteCount -> do
      pure $! IAnnType (8 * byteCount)
    Dwarf.SignedIntType byteCount -> do
      pure $! IAnnType (8 * byteCount)
    Dwarf.FloatType -> do
      pure FloatAnnType
    Dwarf.DoubleType -> do
      pure DoubleAnnType
    Dwarf.LongDoubleType -> do
      throwDwarfTypeError ref "Long double return type is not supported."
    Dwarf.UnsignedCharType -> do
      pure $! IAnnType 8
    Dwarf.SignedCharType -> do
      pure $! IAnnType 8
    Dwarf.ArrayType _ _ -> do
      throwDwarfTypeError ref "Array arguments are not supported."
    Dwarf.PointerType _ Nothing -> do
      pure $ PtrAnnType VoidAnnType
    Dwarf.PointerType _ (Just tr) -> do
      dty <- resolveDwarfTypeRef typeMap tr
      -- Here we distinguish function pointers from other "regular" pointers
      case dty of
        Dwarf.SubroutineTypeF std -> do
          args <- forM (Dwarf.fntypeFormals std) $ \ var -> do
            case Dwarf.varType var of
              Nothing -> throwDwarfTypeError ref "void in argument position is not supported."
              Just tr' -> resolveDwarfType typeMap tr'
          ret <- case Dwarf.fntypeType std of
                  Nothing -> pure VoidAnnType
                  Just retTypeRef -> resolveDwarfType typeMap retTypeRef
          pure $ FunPtrAnnType ret args
        _ -> do
          ty <- resolveDwarfTypeApp typeMap tr dty
          pure $ PtrAnnType ty
    Dwarf.StructType _ -> do
      throwDwarfTypeError ref "Struct arguments are not supported."
    Dwarf.UnionType _ -> do
      throwDwarfTypeError ref "Union arguments are not supported."
    Dwarf.EnumType d ->
      case Dwarf.enumDeclType d of
        Just r -> resolveDwarfType typeMap r
        Nothing -> throwError $ DebugResolveError $ printf "Could not find type for enum at " ++ show ref
    Dwarf.SubroutinePtrType _ -> do
      -- We just use void pointers for now.
      pure $ PtrAnnType VoidAnnType
    Dwarf.TypedefType d -> do
      resolveDwarfType typeMap (Dwarf.typedefType d)
    Dwarf.TypeQualType ann -> do
      case Dwarf.tqaType ann of
        Just r -> resolveDwarfType typeMap r
        Nothing -> throwError $ DebugResolveError $ printf "Could not find type for qualifier at " ++ show ref
    Dwarf.SubroutineTypeF _ -> do
      throwDwarfTypeError ref "Subroutines may not be passed as return values."

-- | Resolve Dwarf arg types
resolveDwarfArgTypes ::
  -- | Origin subprogram if defined
  Maybe Dwarf.Subprogram ->
  Map Dwarf.TypeRef Dwarf.AbsType ->
  -- | Arguments processed so far in reverse order.
  [AnnFunArg] ->
  -- | Number of arguments passed so far.
  Int ->
  [Dwarf.Variable] ->
  ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) (V.Vector AnnFunArg)
resolveDwarfArgTypes _morigin _typeMap prev _cnt [] =
  pure $! V.fromList (reverse prev)
resolveDwarfArgTypes morigin typeMap prev cnt (a : r) = seq cnt $ do
  let nm
        | Dwarf.varName a == "" = "arg" ++ show cnt
        | otherwise = BSC.unpack (Dwarf.nameVal (Dwarf.varName a))
  let mnm
        | Dwarf.varName a == "" = Nothing
        | otherwise = Just (BSC.unpack (Dwarf.nameVal (Dwarf.varName a)))
  tp <-
    case Dwarf.varType a of
      Just ref -> do
        resolveDwarfType typeMap ref
      Nothing -> do
        subOrigin <-
          case morigin of
            Nothing -> throwError $ MissingArgType nm
            Just subOrigin -> pure subOrigin
        varOrig <-
          case Dwarf.varOrigin a of
            Nothing -> throwError $ DebugResolveError "Missing argument abstract origin."
            Just varOrigRef ->
              case Map.lookup varOrigRef (Dwarf.subParamMap subOrigin) of
                Nothing ->
                  throwError $
                    DebugResolveError $
                      printf "Could not find variable origin %s for %s." (show (pretty varOrigRef)) nm
                Just o -> pure o
        -- Get origin ref
        ref <-
          case Dwarf.varType varOrig of
            Nothing -> throwError $ MissingArgType nm
            Just ref -> pure ref
        resolveDwarfType (Dwarf.subTypeMap subOrigin) ref
  let a' = AnnFunArg{funArgName = mnm, funArgType = tp}
  resolveDwarfArgTypes morigin typeMap (a' : prev) (cnt + 1) r

-- | Resolve the type of a Dwarf subprogram
resolveDwarfSubprogramFunType ::
  Dwarf.Subprogram ->
  -- | Origin if subprogram is generated from another.
  Maybe Dwarf.Subprogram ->
  ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) AnnFunType
resolveDwarfSubprogramFunType sub morigin = do
  when (Dwarf.subUnspecifiedParams sub) $
    throwError VarArgsUnsupported
  argTypes <- resolveDwarfArgTypes morigin (Dwarf.subTypeMap sub) [] 0 (Map.elems (Dwarf.subParamMap sub))
  retType <-
    case Dwarf.subRetType sub of
      Nothing ->
        case morigin of
          Nothing -> pure VoidAnnType
          Just origin ->
            case Dwarf.subRetType origin of
              Nothing -> pure VoidAnnType
              Just ref -> resolveDwarfType (Dwarf.subTypeMap origin) ref
      Just ref -> resolveDwarfType (Dwarf.subTypeMap sub) ref

  pure $!
    AnnFunType
      { funRet = retType
      , funArgs = argTypes
      }

-- | @resolveDwarfSubprogramDebugName nm isExt o@ resolve the name to
-- use for subprogram with the given name and offset.
--
-- This returns nothing if name is empty and ext is true or
-- ext if false and the address is empty.
resolveDwarfSubprogramDebugName ::
  -- | Subprogram
  Dwarf.Subprogram ->
  -- | Offset of subprogram.
  Maybe Word64 ->
  Maybe String
resolveDwarfSubprogramDebugName sub moff
  | Dwarf.subExternal sub =
      if Dwarf.subName sub == ""
        then Nothing
        else Just $! BSC.unpack (Dwarf.nameVal (Dwarf.subName sub))
  | otherwise =
      case moff of
        Nothing -> Nothing
        Just o ->
          let
            nmVal :: String
            nmVal
              | Dwarf.subName sub == "" = "Unnamed function"
              | otherwise = BSC.unpack (Dwarf.nameVal (Dwarf.subName sub))
           in
            Just $! printf "%s (0x%x)" nmVal (toInteger o)

-- | Resolve type information from subroutine.
resolveSubprogramType ::
  MemWidth (ArchAddrWidth arch) =>
  -- | Compile unit for this sub program
  Dwarf.CompileUnit ->
  -- | Annotations from source file
  FunTypeMaps (ArchAddrWidth arch) ->
  -- | Dwarf function information
  Dwarf.Subprogram ->
  Maybe (ArchSegmentOff arch) ->
  -- Address
  IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
resolveSubprogramType cu annMap sub entryAddr
  -- Non-defining subprograms are skipped.
  | Dwarf.subIsDeclaration sub = do
      pure annMap
  -- Var args functions have a special usage.
  | Dwarf.subUnspecifiedParams sub = do
      -- Get name as an external symbol
      let
        externalName :: Maybe BSC.ByteString
        externalName = dwarfExternalName sub
      -- Get entry address in terms of memory.
      case resolveDwarfSubprogramDebugName sub (dwarfSubEntry sub) of
        Nothing -> pure annMap
        Just debugName -> do
          unless (funTypeIsDefined annMap externalName entryAddr) $ do
            reoptTypeWarning $ printf "Type error on %s: %s" debugName (showArgResolverError VarArgsUnsupported)
          pure annMap
  | otherwise = do
      -- Get name as an external symbol
      let
        externalName :: Maybe BSC.ByteString
        externalName = dwarfExternalName sub
      -- Get origin if this is an inlined or specialized instance of a source subprogram.
      let emorigin =
            case Dwarf.subOrigin sub of
              Nothing -> Right Nothing
              Just originRef ->
                case Dwarf.lookupSubprogram originRef cu of
                  Nothing -> Left $ "Could not find origin " ++ show (pretty originRef)
                  Just r -> Right (Just r)
      case emorigin of
        Left err -> do
          reoptTypeWarning err
          pure annMap
        Right morigin ->
          case resolveDwarfSubprogramDebugName sub (dwarfSubEntry sub) of
            Nothing -> pure annMap
            Just debugName -> do
              mfunType <- runExceptT $ resolveDwarfSubprogramFunType sub morigin
              case mfunType of
                Left e -> do
                  reoptTypeWarning $ printf "Type error on %s: %s" debugName (showArgResolverError e)
                  pure annMap
                Right funType -> do
                  let (m, warnings) = addNamedFunType annMap debugName externalName entryAddr (ReoptNonvarargFunType funType)
                  mapM_ reoptTypeWarning warnings
                  pure m

-- | This resolve the address of a function given its name and offset.
--
-- This general type is used for eventual support of object files
-- with function sections, where the Dwarf information does not
-- contain address information, and so we use symbol addresses.
--
-- It returns nothing if an address cannot be resolved.
type ResolveAddrFn w = BSC.ByteString -> Word64 -> Maybe (MemSegmentOff w)

-- | Resolve type information from subroutine.
resolveSubprogram ::
  MemWidth (ArchAddrWidth arch) =>
  ResolveAddrFn (ArchAddrWidth arch) ->
  -- | Compile unit for this sub program
  Dwarf.CompileUnit ->
  -- | Annotations from source file
  FunTypeMaps (ArchAddrWidth arch) ->
  -- | Program to resolve
  Dwarf.Subprogram ->
  IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
resolveSubprogram resolveFn cu annMap sub = do
  -- Get entry address in terms of memory.
  entryAddr <-
    case dwarfSubEntry sub of
      Nothing -> pure Nothing
      Just entry -> do
        let dwarfName = Dwarf.subName sub
        let r = resolveFn (Dwarf.nameVal dwarfName) entry
        when (isNothing r) $ do
          let debugName
                | dwarfName == "" = "Unnamed symbol"
                | otherwise = BSC.unpack (Dwarf.nameVal dwarfName)
          reoptTypeWarning $ printf "%s invalid debug address %s." debugName (show entry)
        pure r
  annMap' <- resolveSubprogramType cu annMap sub entryAddr
  case entryAddr of
    Nothing -> pure annMap'
    Just entry -> do
      let val
            | Dwarf.subNoreturn sub = NoReturnFun
            | otherwise = MayReturnFun
      let fn NoReturnFun _ = NoReturnFun
          fn _ NoReturnFun = NoReturnFun
          fn _ _ = MayReturnFun
      pure $ annMap'{noreturnMap = Map.insertWith fn entry val (noreturnMap annMap')}

-- | Add all compile units in plugin
resolveCompileUnits ::
  MemWidth (ArchAddrWidth arch) =>
  ResolveAddrFn (ArchAddrWidth arch) ->
  -- | Map from function names to type info.
  FunTypeMaps (ArchAddrWidth arch) ->
  -- context information
  Maybe (Either String Dwarf.CUContext) ->
  IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
resolveCompileUnits _resolveFn annMap Nothing = do
  pure annMap
resolveCompileUnits _resolveFn annMap (Just (Left e)) = do
  reoptTypeWarning e
  pure annMap
resolveCompileUnits resolveFn annMap (Just (Right ctx)) = do
  let (mcr, warnings) = Dwarf.getCompileUnit ctx
  mapM_ reoptTypeWarning (reverse warnings)
  case mcr of
    Left msg -> do
      reoptTypeWarning msg
      resolveCompileUnits resolveFn annMap (Dwarf.nextCUContext ctx)
    Right cu -> do
      annMap' <- foldlM (resolveSubprogram resolveFn cu) annMap (Dwarf.cuSubprograms cu)
      resolveCompileUnits resolveFn annMap' (Dwarf.nextCUContext ctx)

-- | Populate function type information using debug information.
resolveDebugFunTypes ::
  forall arch r.
  MemWidth (ArchAddrWidth arch) =>
  ResolveAddrFn (ArchAddrWidth arch) ->
  FunTypeMaps (ArchAddrWidth arch) ->
  -- | Elf file for header information
  Elf.ElfHeaderInfo (ArchAddrWidth arch) ->
  IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
resolveDebugFunTypes resolveFn annMap elfInfo = do
  let hdr = Elf.header elfInfo
  let
    secDataMap ::
      Map
        BSC.ByteString
        [ ( Elf.FileRange (Elf.ElfWordType (ArchAddrWidth arch))
          , Elf.ElfSection (Elf.ElfWordType (ArchAddrWidth arch))
          )
        ]
    secDataMap =
      Map.fromListWith
        (++)
        [ (Elf.elfSectionName sec, [(r, sec)])
        | (r, sec) <- V.toList (Elf.headerSections elfInfo)
        ]
  case Map.findWithDefault [] ".debug_info" secDataMap of
    [] -> do
      -- No debug information
      pure annMap
    _ : _ -> do
      incCompLog (ReoptGlobalStepStarted DebugTypeInference)
      let end =
            case Elf.headerData hdr of
              Elf.ELFDATA2LSB -> Dwarf.LittleEndian
              Elf.ELFDATA2MSB -> Dwarf.BigEndian
      sections <- Dwarf.mkSections $ \nm ->
        case Map.findWithDefault [] nm secDataMap of
          [] -> pure BSC.empty
          (_, s) : r -> do
            unless (null r) $ reoptTypeWarning $ printf "Multiple %s sections in Elf file." (BSC.unpack nm)
            pure $! Elf.elfSectionData s
      r <- resolveCompileUnits resolveFn annMap (Dwarf.firstCUContext end sections)
      incCompLog (ReoptGlobalStepFinished DebugTypeInference ())
      pure r
