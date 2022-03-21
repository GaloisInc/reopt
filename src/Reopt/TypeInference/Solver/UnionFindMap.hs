{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Reopt.TypeInference.Solver.UnionFindMap where

import           Control.Lens          (at, (%%~), (%~), (?~))
import           Data.Generics.Product (field)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           GHC.Generics          (Generic)
import           Prettyprinter         (pretty)
import qualified Prettyprinter         as PP

data UnionFindMap k ki v = UnionFindMap
  { ufmEqv  :: Map k ki
  , ufmDefs :: Map k v
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

class Ord k => UFMKeyInfo k ki | ki -> k where
  -- | @compact old new@ does path compression when forwarding pointers
  compact      :: ki -> ki -> ki
  projectKey   :: ki -> k
  injectKey    :: k -> ki
  invertKey    :: k -> ki -> ki

empty :: UFMKeyInfo k ki => UnionFindMap k ki v
empty = UnionFindMap mempty mempty

lookupRep :: UFMKeyInfo k ki => k -> UnionFindMap k ki v -> (ki, UnionFindMap k ki v)
lookupRep k0 = field @"ufmEqv" %%~ go k0
  where
    go k m =
      case Map.lookup k m of
        Nothing  -> (injectKey k, m)
        Just ki' ->
          let (ki'', m') = go (projectKey ki') m
              newki = compact ki' ki''
          in (newki, Map.insert k newki m') -- short circuit next lookup.

-- | Lookup a type variable, returns the representative of the
-- corresponding equivalence class, and the definition for that type
-- var, if any.

lookup :: UFMKeyInfo k ki => k -> UnionFindMap k ki v -> ((ki, Maybe v), UnionFindMap k ki v)
lookup k m = ((ki', Map.lookup (projectKey ki') (ufmDefs m)), m')
  where
    (ki', m') = lookupRep k m

insert :: UFMKeyInfo k ki => k -> v -> UnionFindMap k ki v -> UnionFindMap k ki v
insert k v = field @"ufmDefs" . at k ?~ v

-- | @delete k m@ forgets any definition for @k@, without touching any
-- equivalences.
delete :: UFMKeyInfo k ki => k -> UnionFindMap k ki v -> UnionFindMap k ki v
delete k = field @"ufmDefs" %~ Map.delete k

-- | @unify root leaf@ will make @root@ the new equiv. rep
-- for @leaf@.  Note that both root and leaf should be the reps. of
-- their corresponding equivalence classes, and that only @root@ is allowed a definition.
unify :: UFMKeyInfo k ki => ki -> k -> UnionFindMap k ki v -> UnionFindMap k ki v
unify root leaf = field @"ufmEqv" %~ Map.insert leaf root

-- | Map the representative element onto the class.
eqvClasses :: UFMKeyInfo k ki => UnionFindMap k ki v -> Map k [ki]
eqvClasses um = Map.fromListWith (++) rmap
  where
  rmap = [ (projectKey ki, [invertKey k ki])
         | k <- Map.keys (ufmEqv um)
         , let (ki, _) = lookupRep k um ]

--------------------------------------------------------------------------------
-- instances

instance (UFMKeyInfo k ki, PP.Pretty k, PP.Pretty ki, PP.Pretty v) =>
         PP.Pretty (UnionFindMap k ki v) where
  pretty um = PP.align . PP.vsep $
    [ row "Equivalences" $ [ pretty k PP.<+> ": " PP.<+> PP.list (map pretty v)
                           | (k, v) <- Map.toList (eqvClasses um) ]
    , row "Definitions" $ [ pretty k PP.<+> "→" PP.<+> PP.hang 0 (pretty v)
                          | (k, v) <- Map.toList (ufmDefs um) ]
    ]
    where
      row title entries = title PP.<+> PP.align (PP.list entries)
