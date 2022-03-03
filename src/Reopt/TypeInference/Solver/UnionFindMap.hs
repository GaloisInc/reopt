{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Solver.UnionFindMap where

import           Control.Lens          (at, (%%~), (%~), (?~))
import           Data.Generics.Product (field)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           GHC.Generics          (Generic)
import           Prettyprinter         (pretty)
import qualified Prettyprinter         as PP

data UnionFindMap k v = UnionFindMap
  { ufmEqv  :: Map k k
  , ufmDefs :: Map k v
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

empty :: Ord k => UnionFindMap k v
empty = UnionFindMap mempty mempty

lookupRep :: Ord k => k -> UnionFindMap k v -> (k, UnionFindMap k v)
lookupRep k0 = field @"ufmEqv" %%~ go k0
  where
    go k m =
      case Map.lookup k m of
        Nothing  -> (k, m)
        Just k' ->
          let (k'', m') = go k' m
          in (k'', Map.insert k k'' m') -- short circuit next lookup.

-- | Lookup a type variable, returns the representative of the
-- corresponding equivalence class, and the definition for that type
-- var, if any.

lookup :: Ord k => k -> UnionFindMap k v -> ((k, Maybe v), UnionFindMap k v)
lookup k m = ((k', Map.lookup k' (ufmDefs m)), m')
  where
    (k', m') = lookupRep k m

insert :: Ord k => k -> v -> UnionFindMap k v -> UnionFindMap k v
insert k v = field @"ufmDefs" . at k ?~ v

-- | @delete k m@ forgets any definition for @k@, without touching any
-- equivalences.
delete :: Ord k => k -> UnionFindMap k v -> UnionFindMap k v
delete k = field @"ufmDefs" %~ Map.delete k

-- | @unify root leaf@ will make @root@ the new equiv. rep
-- for @leaf@.  Note that both root and leaf should be the reps. of
-- their corresponding equivalence classes, and that only @root@ is allowed a definition.
unify :: Ord k => k -> k -> UnionFindMap k v -> UnionFindMap k v
unify root leaf = field @"ufmEqv" %~ Map.insert leaf root

-- | Map the representative element onto the class.
eqvClasses :: Ord k => UnionFindMap k v -> Map k [k]
eqvClasses um = Map.fromListWith (++) rmap
  where
  rmap = [ (k', [k]) | k <- Map.keys (ufmEqv um), let (k', _) = lookupRep k um ]

--------------------------------------------------------------------------------
-- instances

instance (Ord k, PP.Pretty k, PP.Pretty v) => PP.Pretty (UnionFindMap k v) where
  pretty um =
    PP.vsep [ row "Equivalences" $ [ pretty k PP.<+> ": " PP.<+> PP.list (map pretty v)
                                   | (k, v) <- Map.toList (eqvClasses um) ]
            , row "Definitions" $ [ pretty k PP.<+> "→" PP.<+> pretty v
                                  | (k, v) <- Map.toList (ufmDefs um) ]
            ]
    where
      row title entries = title PP.<+> PP.list entries

    
  

