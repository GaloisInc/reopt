{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.TypeInference.DebugTypes
  ( resolveDebugFunTypes
  ) where

import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ElfEdit as Elf
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Vector as V
import           Data.Word

import           Prettyprinter (pretty)
import           Text.Printf (printf)

import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import qualified Data.Macaw.Dwarf as Dwarf
import           Data.Macaw.Utils.IncComp


import           Reopt.TypeInference.HeaderTypes
import           Reopt.ArgResolver
import           Reopt.Events
import           Reopt.TypeInference.FunTypeMaps

reoptTypeWarning :: String -> IncCompM (ReoptLogEvent arch) r ()
reoptTypeWarning msg = incCompLog $ ReoptLogEvent DebugTypeInference ReoptWarning msg


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

resolveDwarfTypeRef :: Map Dwarf.TypeRef Dwarf.AbsType
                       -- ^ Logging function for errors
                    -> Dwarf.TypeRef
                    -> ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) Dwarf.TypeApp
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

resolveDwarfType :: Map Dwarf.TypeRef Dwarf.AbsType
                 -> Dwarf.TypeRef
                 -> ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) AnnType
resolveDwarfType typeMap ref = do
  tp <- resolveDwarfTypeRef typeMap ref
  case tp of
    Dwarf.BoolType -> do
      pure $! IAnnType 1
    Dwarf.UnsignedIntType byteCount -> do
      pure $! IAnnType (8*byteCount)
    Dwarf.SignedIntType byteCount -> do
      pure $! IAnnType (8*byteCount)
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
    Dwarf.PointerType _ _ -> do
      -- We just use void pointers for now.
      pure $ PtrAnnType VoidAnnType
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
resolveDwarfArgTypes :: Maybe Dwarf.Subprogram
                        -- ^ Origin subprogram if defined
                      -> Map Dwarf.TypeRef Dwarf.AbsType
                      -> [AnnFunArg] -- ^ Arguments processed so far in reverse order.
                      -> Int -- ^ Number of arguments passed so far.
                      -> [Dwarf.Variable]
                      -> ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) (V.Vector AnnFunArg)
resolveDwarfArgTypes _morigin _typeMap prev _cnt [] =
  pure $! V.fromList (reverse prev)
resolveDwarfArgTypes morigin typeMap prev cnt (a:r) = seq cnt $ do
  let nm | Dwarf.varName a == "" = "arg" ++ show cnt
         | otherwise = BSC.unpack (Dwarf.nameVal (Dwarf.varName a))
  let mnm | Dwarf.varName a == "" = Nothing
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
            Nothing -> throwError $ DebugResolveError $ "Missing argument abstract origin."
            Just varOrigRef ->
              case Map.lookup varOrigRef (Dwarf.subParamMap subOrigin) of
                Nothing ->
                  throwError $ DebugResolveError $
                    printf "Could not find variable origin %s for %s." (show (pretty varOrigRef)) nm
                Just o -> pure o
        -- Get origin ref
        ref <-
          case Dwarf.varType varOrig of
            Nothing -> throwError $ MissingArgType nm
            Just ref -> pure ref
        resolveDwarfType (Dwarf.subTypeMap subOrigin) ref
  let a' = AnnFunArg { funArgName = mnm, funArgType = tp }
  resolveDwarfArgTypes morigin typeMap (a':prev) (cnt+1) r

-- | Resolve the type of a Dwarf subprogram
resolveDwarfSubprogramFunType :: Dwarf.Subprogram
                              -> Maybe Dwarf.Subprogram -- ^ Origin if subprogram is generated from another.
                              -> ExceptT ArgResolverError (IncCompM (ReoptLogEvent w) r) AnnFunType
resolveDwarfSubprogramFunType sub morigin = do
  when (Dwarf.subUnspecifiedParams sub) $
    throwError $ VarArgsUnsupported
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

  pure $! AnnFunType
    { funRet    = retType
    , funArgs  = argTypes
    , funVarArg = False
    }

-- | @resolveDwarfSubprogramDebugName nm isExt o@ resolve the name to
-- use for subprogram with the given name and offset.
--
-- This returns nothing if name is empty and ext is true or
-- ext if false and the address is empty.
resolveDwarfSubprogramDebugName :: Dwarf.Subprogram -- ^ Subprogram
                                -> Maybe Word64 -- ^ Offset of subprogram.
                                -> Maybe String
resolveDwarfSubprogramDebugName sub moff
  | Dwarf.subExternal sub =
    if Dwarf.subName sub == "" then
      Nothing
     else
      Just $! BSC.unpack (Dwarf.nameVal (Dwarf.subName sub))
  | otherwise =
    case moff of
      Nothing -> Nothing
      Just o ->
        let nmVal :: String
            nmVal | Dwarf.subName sub == "" = "Unnamed function"
                  | otherwise = BSC.unpack (Dwarf.nameVal (Dwarf.subName sub))
         in Just $! printf "%s (0x%x)" nmVal (toInteger o)

-- | Resolve type information from subroutine.
resolveSubprogramType :: Dwarf.CompileUnit
                      -- ^ Compile unit for this sub program
                      -> FunTypeMaps (ArchAddrWidth arch)
                      -- ^ Annotations from source file
                      -> Dwarf.Subprogram
                      -- ^ Dwarf function information
                      -> Maybe (ArchSegmentOff arch)
                         -- Address
                      -> IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
resolveSubprogramType cu annMap sub entryAddr
  -- Non-defining subprograms are skipped.
  | Dwarf.subIsDeclaration sub = do
      pure annMap
    -- Var args functions have a special usage.
  | Dwarf.subUnspecifiedParams sub = do
      -- Get name as an external symbol
      let externalName :: Maybe BSC.ByteString
          externalName = dwarfExternalName sub
      -- Get entry address in terms of memory.
      case resolveDwarfSubprogramDebugName sub (dwarfSubEntry sub) of
        Nothing -> pure annMap
        Just debugName -> do
          when (not (funTypeIsDefined annMap externalName entryAddr)) $ do
            reoptTypeWarning $ printf "Type error on %s: %s" debugName (showArgResolverError VarArgsUnsupported)
          pure annMap
  | otherwise = do
      -- Get name as an external symbol
      let externalName :: Maybe BSC.ByteString
          externalName = dwarfExternalName sub
      -- Get origin if this is an inlined or specialized instance of a source subprogram.
      let emorigin =
            case Dwarf.subOrigin sub of
              Nothing -> Right Nothing
              Just originRef ->
                case Map.lookup originRef (Dwarf.cuSubprogramMap cu) of
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
                  pure $ annMap
                Right funType -> do
                  let (m, warnings) = addNamedFunType annMap debugName externalName entryAddr (ReoptNonvarargFunType funType)
                  mapM_ reoptTypeWarning warnings
                  pure m


-- | Resolve type information from subroutine.
resolveSubprogram :: Dwarf.CompileUnit
                  -- ^ Compile unit for this sub program
                  -> FunTypeMaps (ArchAddrWidth arch)
                  -- ^ Annotations from source file
                  -> Dwarf.Subprogram
                  -- ^ Elf file for header information
                  -> IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
resolveSubprogram cu annMap sub = do
  -- Get entry address in terms of memory.
  entryAddr <-
    case dwarfSubEntry sub of
      Nothing -> pure Nothing
      Just entry -> do
        let dwarfName = Dwarf.subName sub
        let r = dwarfAddrResolve annMap (Dwarf.nameVal dwarfName) entry
        when (isNothing r) $ do
          let debugName | dwarfName == "" = "Unnamed symbol"
                        | otherwise = BSC.unpack (Dwarf.nameVal dwarfName)
          reoptTypeWarning $ printf "%s invalid debug address %s." debugName (show entry)
        pure r
  annMap' <- resolveSubprogramType cu annMap sub entryAddr
  case entryAddr of
    Nothing -> pure annMap'
    Just entry -> do
      let val | Dwarf.subNoreturn sub = NoReturnFun
              | otherwise = MayReturnFun
      let fn NoReturnFun _ = NoReturnFun
          fn _ NoReturnFun = NoReturnFun
          fn _ _ = MayReturnFun
      pure $ annMap' { noreturnMap = Map.insertWith fn entry val (noreturnMap annMap') }

-- | Add all compile units in plugin
resolveCompileUnits :: FunTypeMaps (ArchAddrWidth arch)
                    -- ^ Map from function names to type info.
                    -> Maybe (Either String Dwarf.CUContext)
                    -- ^ Elf file for header information
                    -> IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
resolveCompileUnits annMap Nothing = do
  pure annMap
resolveCompileUnits annMap (Just (Left e)) = do
  reoptTypeWarning e
  pure annMap
resolveCompileUnits annMap (Just (Right ctx)) = do
  let (mcr, warnings) = Dwarf.getCompileUnit ctx
  mapM_ reoptTypeWarning (reverse warnings)
  case mcr of
    Left msg -> do
      reoptTypeWarning msg
      resolveCompileUnits annMap (Dwarf.nextCUContext ctx)
    Right cu -> do
      annMap' <- foldlM (resolveSubprogram cu) annMap (Dwarf.cuSubprograms cu)
      resolveCompileUnits annMap' (Dwarf.nextCUContext ctx)

-- | Populate function type information using debug information.
resolveDebugFunTypes :: forall arch r
                     .  FunTypeMaps (ArchAddrWidth arch)
                     -- ^ Annotations from source file
                     -> Elf.ElfHeaderInfo (ArchAddrWidth arch)
                     -- ^ Elf file for header information
                     -> IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
resolveDebugFunTypes annMap elfInfo = do
  let hdr = Elf.header elfInfo
  let secDataMap :: Map BSC.ByteString [( Elf.FileRange  (Elf.ElfWordType (ArchAddrWidth arch))
                                       , Elf.ElfSection (Elf.ElfWordType (ArchAddrWidth arch))
                                       )]
      secDataMap = Map.fromListWith (++)
        [ (Elf.elfSectionName sec, [(r,sec)])
        | (r,sec) <- V.toList (Elf.headerSections elfInfo)
        ]
  case Map.findWithDefault [] ".debug_info" secDataMap of
    [] -> do
      -- No debug information
      pure annMap
    _:_ -> do
      incCompLog (ReoptStepStarted DebugTypeInference)
      let end =
            case Elf.headerData hdr of
              Elf.ELFDATA2LSB -> Dwarf.LittleEndian
              Elf.ELFDATA2MSB -> Dwarf.BigEndian
      sections <- Dwarf.mkSections $ \nm ->
        case Map.findWithDefault [] nm secDataMap of
          [] -> pure BSC.empty
          (_, s):r -> do
            when (not (null r)) $ reoptTypeWarning $ printf "Multiple %s sections in Elf file." (BSC.unpack nm)
            pure $! Elf.elfSectionData s
      r <- resolveCompileUnits annMap (Dwarf.firstCUContext end sections)
      incCompLog (ReoptStepFinished DebugTypeInference ())
      pure r
