{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Reopt.TypeInference.Solver.UnionFindMap where

import Control.Lens (at, (%~), (?~), (^.), (.~))
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Prettyprinter (pretty)
import Prettyprinter qualified as PP

-- | A `RevocableUnionFindMap` is a bit like a union-find structure, but we need
-- a way of revoking certain equalities that proved out to be incorrect.
--
-- Intuitively, this is not possible with how forgetful the union-find union
-- operation is: once two sets are merged together with one representative, we
-- no longer know how to undo this merge.
--
-- Our idea is to have two running graphs:
--
-- * The "real" graph, contains those actual equalities we have been told about,
-- marked with a revocation identifier,
--
-- * The "fast" graph implements what you'd think of as the union-find
-- algorithm, that is, it contains shortcuts to representatives for the
-- equivalence class, along with the sets of all revocation identifiers that
-- this shortcut relies upon.
--
-- The idea is then to populate and rely on the "fast" graph until a union is
-- revoked, at which point we delete all shortcuts that relied on it, which
-- makes us fall back to using the "real" map.
--
-- The type variables mean:
--
-- * `r` is the datatype that identifies unions, in order to indicate them for
-- revocation, as such, it ought to support an `Ord` instance.
--
-- * `k` is the types of key in the union-find map.
--
-- * `ki` is the type of value in the union-find map.  While you may expect this
-- to be the same as `k`, for row expressions we do something a little different
-- where keys and values differ.
--
-- * `v` is the type of "metadata" we want to attach to a given equivalence
-- class.
data RevocableUnionFindMap r k ki v = RevocableUnionFindMap
  { rufmReal :: [(r, k, ki)]
  , rufmFast :: Map k (Set.Set r, ki)
  , rufmDefs :: Map k v
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

empty :: UFMKeyInfo k ki => RevocableUnionFindMap r k ki v
empty = RevocableUnionFindMap mempty mempty mempty

-- | Looks up the representative of an equivalence class in the "real" map.
-- Returns the representative, alongside with the set of revocation indices
-- involved in this lookup.
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

-- | Given a predicate that decides whether edges should be revoked, returns a
-- map where all such edges have been removed from both the real map and the
-- fast one.
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

-- | This is the crux of the "revocable union-find" algorithm.  It looks up the
-- representative of an equivalence class, ideally in the "fast" map, but
-- reverting to the "real" map otherwise, compacting and collecting revocation
-- identifiers along the way.
--
-- TODO: Short of proving this correct, I think we at least ought to be able to
-- {small,quick}check that the value returned by `lookupRepFast` agrees with the
-- value returned by `lookupRepReal`.
lookupRepFast ::
  forall r k ki v.
  (Ord r, PP.Pretty k, UFMKeyInfo k ki) =>
  k ->
  RevocableUnionFindMap r k ki v ->
  (Set.Set r, (ki, RevocableUnionFindMap r k ki v))
lookupRepFast k0 m0 =
  let (rs, (ki, newFast)) = go k0 (m0 ^. field @"rufmFast")
   in (rs, (ki, (field @"rufmFast" .~ newFast) m0))
  where
    go :: k -> Map k (Set.Set r, ki) -> (Set.Set r, (ki, Map k (Set.Set r, ki)))
    go k m =
      case Map.lookup k m of
        Nothing ->
          let (rs, ki) = lookupRepReal (m0 ^. field @"rufmReal") k
          in trace "Nothing" (rs, (ki, Map.insert k (rs, ki) m))
        Just (rs, ki') ->
          let k' = projectKey ki' in
          if k' == k
            then trace "Done" (rs, (ki', m))
            else
              let (rs', (ki'', m')) = go k' m
                  newrs = Set.union rs rs'
                  newki = compact ki' ki''
              in trace (show (PP.pretty k)) (newrs, (newki, Map.insert k (newrs, newki) m')) -- short circuit next lookup.

-- | Lookup a type variable, returns the representative of the corresponding
-- equivalence class, and the definition for that type var, if any.
lookup ::
  (Ord r, PP.Pretty k, UFMKeyInfo k ki) =>
  k ->
  RevocableUnionFindMap r k ki v ->
  ((ki, Maybe v), RevocableUnionFindMap r k ki v)
lookup k m =
  trace ("Looking up " <> show (PP.pretty k))
  ((ki', Map.lookup (projectKey ki') (rufmDefs m)), m')
 where
  (_, (ki', m')) = lookupRepFast k m

-- | @insert k v m@ inserts a definition for the key `k` as `v` in the revocable
-- union-find map `m`.
insert ::
  (Show k, Show v, UFMKeyInfo k ki) =>
  k -> v ->
  RevocableUnionFindMap r k ki v ->
  RevocableUnionFindMap r k ki v
insert k v =
  trace ("Inserting " <> show k <> " mapping to " <> show v) $
  field @"rufmDefs" . at k ?~ v

-- | @delete k m@ forgets any definition for @k@, without touching any
-- equivalences.
delete ::
  UFMKeyInfo k ki =>
  k -> RevocableUnionFindMap r k ki v -> RevocableUnionFindMap r k ki v
delete k = field @"rufmDefs" %~ Map.delete k

-- | @unify root leaf@ will make @root@ the new equiv. rep for @leaf@.  Note
-- that both root and leaf should be the reps. of their corresponding
-- equivalence classes, and that only @root@ is allowed a definition.
unify ::
  (PP.Pretty k, PP.Pretty ki, PP.Pretty r, UFMKeyInfo k ki) =>
  r -> ki -> k -> RevocableUnionFindMap r k ki v -> RevocableUnionFindMap r k ki v
unify r root leaf =
  trace (show $ PP.hsep ["Unifying", PP.pretty root, "and", PP.pretty leaf]) $
  field @"rufmReal" %~ Map.insert leaf (r, root)

-- | Map the representative element onto the class.
eqvClasses ::
  (Ord r, PP.Pretty k, PP.Pretty ki, PP.Pretty r, PP.Pretty v, UFMKeyInfo k ki) =>
  RevocableUnionFindMap r k ki v ->
  Map k [ki]
eqvClasses um = Map.fromListWith (++) rmap
 where
  rmap =
    [ (projectKey ki, [invertKey k ki])
    | k <- Map.keys (rufmFast um)
    , let (_, (ki, _)) = lookupRepFast k um
    ]

--------------------------------------------------------------------------------
-- instances

instance
  (Ord r, PP.Pretty k, PP.Pretty ki, PP.Pretty r, PP.Pretty v, UFMKeyInfo k ki) =>
  PP.Pretty (RevocableUnionFindMap r k ki v)
  where
    pretty um =
      let row title entries = title PP.<+> PP.align (PP.list entries) in
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

class Ord k => UFMKeyInfo k ki | ki -> k where
  -- | @compact old new@ does path compression when forwarding pointers
  compact :: ki -> ki -> ki

  projectKey :: ki -> k
  injectKey :: k -> ki
  invertKey :: k -> ki -> ki
