{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Reopt.TypeInference.Solver.UnionFindMap where

import Control.Lens (at, (%~), (?~), (^.))
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Prettyprinter (pretty)
import Prettyprinter qualified as PP

data RevocableUnionFindMap r k ki v = RevocableUnionFindMap
  { rufmReal :: Map k (r, ki)
  , rufmFast :: Map k (Set.Set r, ki)
  , rufmDefs :: Map k v
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

empty :: UFMKeyInfo k ki => RevocableUnionFindMap r k ki v
empty = RevocableUnionFindMap mempty mempty mempty

lookupRepReal ::
  (Ord k, Ord r, UFMKeyInfo k ki) =>
  Map k (r, ki) ->
  k ->
  (Set.Set r, ki)
lookupRepReal m = go
 where
  go k =
    case Map.lookup k m of
      Nothing -> (Set.empty, injectKey k)
      Just (r, ki) ->
        let (rs, ki') = go (projectKey ki)
         in (Set.insert r rs, compact ki ki')

{- | Given a predicate that decides whether edges should be revoked, returns a
 - map where all such edges have been removed from both the real map and the fast
 - one.
-}
revoke ::
  Ord r =>
  (Set.Set r -> Bool) ->
  RevocableUnionFindMap r k ki v ->
  RevocableUnionFindMap r k ki v
revoke isRevoked =
  (field @"rufmReal" %~ Map.filter (not . isRevokedTraced . Set.singleton . fst))
    . (field @"rufmFast" %~ Map.filter (not . isRevokedTraced . fst))
 where
  isRevokedTraced s = isRevoked s && trace "Revoked" True

lookupRepFast ::
  (Ord r, UFMKeyInfo k ki) =>
  k ->
  RevocableUnionFindMap r k ki v ->
  (Set.Set r, (ki, RevocableUnionFindMap r k ki v))
lookupRepFast k0 m0 =
  let (rs, ki) = lookupRepReal (m0 ^. field @"rufmReal") k0
   in (rs, (ki, m0))

-- (field @"rufmFast" %%~ (snd . go k0)) m0
-- where
--   mReal = m0 ^. field @"rufmReal"
--   -- Locally, we also return a set of revocation values, capturing all the
--   -- ones that were used in giving this answer.
--   go k m =
--     case Map.lookup k m of
--       Nothing ->
--         let (rs, ki) = lookupRepReal mReal k
--         in (rs, (ki, m))
--       Just (rs, ki') ->
--         let (rs', (ki'', m')) = go (projectKey ki') m
--             newrs = Set.union rs rs'
--             newki = compact ki' ki''
--         in (newrs, (newki, Map.insert k (newrs, newki) m')) -- short circuit next lookup.

{- | Lookup a type variable, returns the representative of the corresponding
 - equivalence class, and the definition for that type var, if any.
-}
lookup ::
  (Ord r, Show k, Show ki, UFMKeyInfo k ki) =>
  k ->
  RevocableUnionFindMap r k ki v ->
  ((ki, Maybe v), RevocableUnionFindMap r k ki v)
lookup k m =
  ((ki', Map.lookup (projectKey ki') (rufmDefs m)), m')
 where
  (_, (ki', m')) = lookupRepFast k m

{- | @insert k v m@ inserts a definition for the key `k` as `v` in the revocable
 - union-find map `m`.
-}
insert ::
  UFMKeyInfo k ki =>
  k ->
  v ->
  RevocableUnionFindMap r k ki v ->
  RevocableUnionFindMap r k ki v
insert k v = field @"rufmDefs" . at k ?~ v

{- | @delete k m@ forgets any definition for @k@, without touching any
 equivalences.
-}
delete :: UFMKeyInfo k ki => k -> RevocableUnionFindMap r k ki v -> RevocableUnionFindMap r k ki v
delete k = field @"rufmDefs" %~ Map.delete k

{- | @unify root leaf@ will make @root@ the new equiv. rep for @leaf@.  Note
 that both root and leaf should be the reps. of their corresponding equivalence
 classes, and that only @root@ is allowed a definition.
-}
unify :: UFMKeyInfo k ki => r -> ki -> k -> RevocableUnionFindMap r k ki v -> RevocableUnionFindMap r k ki v
unify r root leaf = field @"rufmReal" %~ Map.insert leaf (r, root)

-- | Map the representative element onto the class.
eqvClasses ::
  (Ord r, UFMKeyInfo k ki) =>
  RevocableUnionFindMap r k ki v ->
  Map k [ki]
eqvClasses um = Map.fromListWith (++) rmap
 where
  rmap =
    [ (projectKey ki, [invertKey k ki])
    | k <- Map.keys (rufmReal um)
    , let (_, (ki, _)) = lookupRepFast k um
    ]

--------------------------------------------------------------------------------
-- instances

instance
  (Ord r, UFMKeyInfo k ki, PP.Pretty k, PP.Pretty ki, PP.Pretty v) =>
  PP.Pretty (RevocableUnionFindMap r k ki v)
  where
  pretty um =
    PP.align . PP.vsep $
      [ row "Equivalences" $
          [ pretty k PP.<+> ": " PP.<+> PP.list (map pretty v)
          | (k, v) <- Map.toList (eqvClasses um)
          ]
      , row "Definitions" $
          [ pretty k PP.<+> "→" PP.<+> PP.hang 0 (pretty v)
          | (k, v) <- Map.toList (rufmDefs um)
          ]
      ]
   where
    row title entries = title PP.<+> PP.align (PP.list entries)

-- data UnionFindMap k ki v = UnionFindMap
--   { ufmEqv  :: Map k ki
--   , ufmDefs :: Map k v
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

class Ord k => UFMKeyInfo k ki | ki -> k where
  -- | @compact old new@ does path compression when forwarding pointers
  compact :: ki -> ki -> ki

  projectKey :: ki -> k
  injectKey :: k -> ki
  invertKey :: k -> ki -> ki

-- empty :: UFMKeyInfo k ki => UnionFindMap k ki v
-- empty = UnionFindMap mempty mempty

-- lookupRep :: UFMKeyInfo k ki => k -> UnionFindMap k ki v -> (ki, UnionFindMap k ki v)
-- lookupRep k0 = field @"ufmEqv" %%~ go k0
--   where
--     go k m =
--       case Map.lookup k m of
--         Nothing  -> (injectKey k, m)
--         Just ki' ->
--           let (ki'', m') = go (projectKey ki') m
--               newki = compact ki' ki''
--           in (newki, Map.insert k newki m') -- short circuit next lookup.

-- -- | Lookup a type variable, returns the representative of the
-- -- corresponding equivalence class, and the definition for that type
-- -- var, if any.

-- lookup :: UFMKeyInfo k ki => k -> UnionFindMap k ki v -> ((ki, Maybe v), UnionFindMap k ki v)
-- lookup k m = ((ki', Map.lookup (projectKey ki') (ufmDefs m)), m')
--   where
--     (ki', m') = lookupRep k m

-- insert :: UFMKeyInfo k ki => k -> v -> UnionFindMap k ki v -> UnionFindMap k ki v
-- insert k v = field @"ufmDefs" . at k ?~ v

-- -- | @delete k m@ forgets any definition for @k@, without touching any
-- -- equivalences.
-- delete :: UFMKeyInfo k ki => k -> UnionFindMap k ki v -> UnionFindMap k ki v
-- delete k = field @"ufmDefs" %~ Map.delete k

-- -- | @unify root leaf@ will make @root@ the new equiv. rep
-- -- for @leaf@.  Note that both root and leaf should be the reps. of
-- -- their corresponding equivalence classes, and that only @root@ is allowed a definition.
-- unify :: UFMKeyInfo k ki => ki -> k -> UnionFindMap k ki v -> UnionFindMap k ki v
-- unify root leaf = field @"ufmEqv" %~ Map.insert leaf root

-- -- | Map the representative element onto the class.
-- eqvClasses :: UFMKeyInfo k ki => UnionFindMap k ki v -> Map k [ki]
-- eqvClasses um = Map.fromListWith (++) rmap
--   where
--   rmap = [ (projectKey ki, [invertKey k ki])
--          | k <- Map.keys (ufmEqv um)
--          , let (ki, _) = lookupRep k um ]

-- --------------------------------------------------------------------------------
-- -- instances

-- instance (UFMKeyInfo k ki, PP.Pretty k, PP.Pretty ki, PP.Pretty v) =>
--          PP.Pretty (UnionFindMap k ki v) where
--   pretty um = PP.align . PP.vsep $
--     [ row "Equivalences" $ [ pretty k PP.<+> ": " PP.<+> PP.list (map pretty v)
--                            | (k, v) <- Map.toList (eqvClasses um) ]
--     , row "Definitions" $ [ pretty k PP.<+> "→" PP.<+> PP.hang 0 (pretty v)
--                           | (k, v) <- Map.toList (ufmDefs um) ]
--     ]
--     where
--       row title entries = title PP.<+> PP.align (PP.list entries)
