{-|
Type declarations used for type inference.
-}
{-# OPTIONS_GHC -Wwarn #-}
module Reopt.TypeInference.FunTypeMaps
  ( QualifiedSymbolName(..)
  , ReoptFunType(..)
    -- * Symbol addr Map
  , SymAddrMap(..)
  , symAddrMapEmpty
  , symAddrMapInsert
  , symAddrMapLookup
  , SymAddrMapLookupError(..)
  , symAddrMapContents
  , getAddrSymMap
    -- * FunTypeMaps
  , FunTypeMaps(..)
  , funTypeMapsEmpty
  , funTypeIsDefined
  , addNamedFunType
  ) where

import           Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ElfEdit.Prim as Elf
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Text.Printf (printf)

import           Data.Macaw.Discovery (NoReturnFunStatus(..))
import           Data.Macaw.Memory ( MemSegmentOff )

import           Reopt.TypeInference.HeaderTypes ( AnnFunType )


------------------------------------------------------------------------
-- QualifiedSymbolName

-- | Name of a symbol along with its visibility
data QualifiedSymbolName
   = QualifiedSymbolName
   { qsnBytes :: !BS.ByteString
     -- ^ Bytestring
   , qsnGlobal :: !Bool
     -- ^ Flag indicating if this is a global symbol
     --
     -- Global symbols should be unique for a binary while non-global
     -- symbols are only unique for a compilation unit.
   } deriving (Show)

mkQualifiedSymbolName :: Elf.SymtabEntry BS.ByteString w
                      -> QualifiedSymbolName
mkQualifiedSymbolName ste =
  QualifiedSymbolName { qsnBytes  = Elf.steName ste
                      , qsnGlobal = Elf.steBind ste == Elf.STB_GLOBAL
                      }

-- | @mergeName new old@ picks the symbol to use when two symbols @new@ and @old@
-- have the same address, and we need a sensible default.
mergeName :: QualifiedSymbolName -> QualifiedSymbolName -> QualifiedSymbolName
mergeName new old =
  case (qsnGlobal new, qsnGlobal old) of
    -- Replace local symbols with global symbols.
    (True, False) -> new
    -- Otherwise use old symbol.
    _ -> old


--------------------------------------------------------------------------------
-- SymAddrMap

-- | Maintain symbol/address name mappings.
data SymAddrMap w =
  SymAddrMap { samNameMap :: !(Map BS.ByteString (Set.Set (MemSegmentOff w)))
               -- ^ Map from global and local symbol names to their address.
             , samAddrMap :: !(Map (MemSegmentOff w) QualifiedSymbolName)
               -- ^ Map from address to the symbol to use for that address.
               --
               -- In the case where multiple symbols have the same address, we
               -- use the first symbol with that address in the symbol table but
               -- prioritze global symbols over local symbols.
             }

-- | Return list of names and addresses stored in sym addr map
symAddrMapContents :: SymAddrMap w -> [(BS.ByteString, MemSegmentOff w)]
symAddrMapContents m = [ (nm, a)
                       | (nm, s) <- Map.toList (samNameMap m)
                       , a <- Set.toList s
                       ]

getAddrSymMap :: SymAddrMap w -> Map (MemSegmentOff w) BS.ByteString
getAddrSymMap sam = fmap qsnBytes (samAddrMap sam)

-- | Empty symbol address map
symAddrMapEmpty :: SymAddrMap w
symAddrMapEmpty = SymAddrMap { samNameMap = Map.empty
                             , samAddrMap = Map.empty
                             }

-- | Symbol address map insertion.
symAddrMapInsert :: Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w)
                 -> MemSegmentOff w
                 -> SymAddrMap w
                 -> SymAddrMap w
symAddrMapInsert sym addr sam = seq addr $
  let qnm = mkQualifiedSymbolName sym
      nmMap' = Map.insertWith (\_new -> Set.insert addr)
                              (qsnBytes qnm)
                              (Set.singleton addr)
                              (samNameMap sam)
      addrMap' = Map.insertWith mergeName addr qnm (samAddrMap sam)
   in seq qnm $ SymAddrMap { samNameMap = nmMap'
                           , samAddrMap = addrMap'
                           }

-- | Error code if @symAddrMapLookup@ fails.
data SymAddrMapLookupError
   = SymAddrMapNotFound
   | SymAddrMapAmbiguous

-- | Lookup entry in symbol to address map.
symAddrMapLookup :: SymAddrMap w -> BS.ByteString -> Either SymAddrMapLookupError (MemSegmentOff w)
symAddrMapLookup sam nm =
  let s = Map.findWithDefault Set.empty nm (samNameMap sam)
   in case Set.size s of
       0 -> Left SymAddrMapNotFound
       1 -> Right (Set.findMin s)
       _ -> Left SymAddrMapAmbiguous

instance Semigroup (SymAddrMap w) where
  x <> y = SymAddrMap
           { samNameMap = (samNameMap x) <> (samNameMap y)
           , samAddrMap   = (samAddrMap x) <> (samAddrMap y)
           }


---------------------------------------------------------------------------------
-- ReoptFunType

-- | This describes the arguments to a function using the type system
-- internally maintained by Reopt.
data ReoptFunType
   = ReoptNonvarargFunType !AnnFunType
   | ReoptPrintfFunType !Int
     -- ^ A function that is like printf where the last non-vararg
     -- argument is a string and subsequent arguments are inferred
     -- from it.
     --
     -- The int denotes the number of 64-bit bitvectors previously.
   | ReoptOpenFunType
     -- ^ Open
   | ReoptUnsupportedFunType
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- FunTypeMaps


-- | Function type information parsed from annotations and debug information.
data FunTypeMaps w =
  FunTypeMaps { nameToAddrMap :: !(SymAddrMap w)
                -- ^ Map from symbol names to the address.
              , nameTypeMap :: !(Map BS.ByteString ReoptFunType)
                -- ^ Map from external undefined symbol names to type.
              , addrTypeMap :: !(Map (MemSegmentOff w) ReoptFunType)
                -- ^ Map from code addresses that are start of function
                -- to type.
              , noreturnMap :: !(Map (MemSegmentOff w) NoReturnFunStatus)
              }

-- | Empty function type information.
funTypeMapsEmpty :: FunTypeMaps w
funTypeMapsEmpty = FunTypeMaps symAddrMapEmpty Map.empty Map.empty Map.empty

-- | Returnm true if the function name or address is known by Reopt.
funTypeIsDefined :: FunTypeMaps w -- ^ Current type map information
                 -> Maybe BS.ByteString -- ^ External name of function if not defined
                 -> Maybe (MemSegmentOff w)
                 -> Bool
funTypeIsDefined funTypeMaps msym maddr = do
  let symDef = case msym of
                 Nothing -> True
                 Just sym -> Map.member sym (nameTypeMap funTypeMaps)
      addrDef = case maddr of
                  Nothing -> True
                  Just addr -> Map.member addr (addrTypeMap funTypeMaps)
   in symDef && addrDef

-- | Add a type to a map while issuing a warning if the type is incompatible.
addCheckExisting :: Ord k
                 => String
                 -> k
                 -> ReoptFunType
                 -> Map k ReoptFunType
                 -> State [String] (Map k ReoptFunType)
addCheckExisting nm k v m =
  case Map.lookup k m of
    Nothing ->
      pure $! Map.insert k v m
    Just pv -> do
      when (pv /= v) $ do
        modify (printf "%s assigned incompatible types.\nPrev:\n%s\nNew:\n%s" nm (show pv) (show v) :)
      pure m



-- | Add a new function type to a function
addNamedFunType :: FunTypeMaps w -- ^ Current type map information
                -> String -- ^ Name of entry for logging purposes.
                -> Maybe BS.ByteString -- ^ External name of function if not defined
                -> Maybe (MemSegmentOff w)
                -> ReoptFunType
                -> (FunTypeMaps w, [String])
addNamedFunType funTypeMaps loggingName msym maddr reoptFunType = flip runState [] $ do
  ntm <-
    case msym of
      Nothing ->
        pure $! nameTypeMap funTypeMaps
      Just sym -> do
        addCheckExisting loggingName sym reoptFunType (nameTypeMap funTypeMaps)
  atm <-
    case maddr of
      Nothing -> do
        pure $! addrTypeMap funTypeMaps
      Just addr -> do
        let nm = loggingName ++ " address"
        addCheckExisting nm addr reoptFunType (addrTypeMap funTypeMaps)
  pure $! funTypeMaps { nameTypeMap = ntm, addrTypeMap = atm }
