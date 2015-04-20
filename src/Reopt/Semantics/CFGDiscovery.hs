------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.CFGDiscovery
-- Description      : Control Flow Graph discovery support
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Simon Winwood <sjw@galois.com>
-- Stability        : provisional
--
-- This contains an implementation of a CFG discovery algorithm based
-- upon an interleaved abstract interpretation (currently unsound)
------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.Semantics.CFGDiscovery
       ( cfgFromAddress
       ) where

import           Control.Applicative ( (<$>) )
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Foldable as Fold
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust, fromMaybe, catMaybes)
import           Data.Monoid (mappend, mempty)
import           Data.Parameterized.NatRepr
import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import           Data.Word
import           Debug.Trace
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen (pretty)
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Reopt.Memory
import           Reopt.Semantics.Implementation
import           Reopt.Semantics.Representation
import qualified Reopt.Semantics.StateNames as N
import           Reopt.Semantics.Types
import qualified Reopt.Semantics.WorkList as WL

------------------------------------------------------------------------
-- Abstract states

class Eq d => AbsDomain d where
  -- | The top element
  top :: d

  -- | A partial ordering over d.  forall x. x `leq` top
  leq :: d -> d -> Bool

  -- | Least upper bound (always defined, as we have top)
  lub :: d -> d -> d

  -- | Join the old and new states and return the updated state iff
  -- the result is larger than the old state.
  joinD :: d -> d -> Maybe d
  joinD old new
    | new `leq` old = Nothing
    | otherwise     = Just $ lub old new

------------------------------------------------------------------------
-- Interpreter state

-- FIXME: types/sizes?
data AbsValue (tp :: Type)
   = AbsValue !(Set Integer)
   | TopV

instance Eq (AbsValue tp) where
  AbsValue x == AbsValue y = x == y
  TopV == TopV = True
  _ == _ = False

abstractSingleton :: Integer -> AbsValue tp
abstractSingleton = AbsValue . S.singleton

concretize :: AbsValue tp -> Maybe (Set Integer)
concretize TopV         = Nothing
concretize (AbsValue s) = Just s

newtype AbsValue' (tp :: Type) = AbsValue' { unAbsValue' :: AbsValue tp }
  deriving (AbsDomain)

instance Eq (AbsValue' a) where
  AbsValue' x == AbsValue' y = x == y

instance EqF AbsValue' where
  eqF = (==)

emptyAbsValue :: AbsValue tp
emptyAbsValue = AbsValue S.empty

newtype AbsBlockState = AbsBlockState { _absX86State :: X86State' AbsValue' }
  deriving Eq

absX86State :: Simple Lens AbsBlockState (X86State' AbsValue')
absX86State = lens _absX86State (\s v -> s { _absX86State = v })

instance AbsDomain (AbsValue tp) where
  top = TopV

  leq _ TopV = True
  leq TopV _ = False
  leq (AbsValue v) (AbsValue v') = v `S.isSubsetOf` v'

  lub _ TopV = TopV
  lub TopV _ = TopV
  lub (AbsValue v) (AbsValue v') = AbsValue $ v `S.union` v'

instance AbsDomain AbsBlockState where
  top = AbsBlockState $ mkX86State (\_ -> top)
  leq (AbsBlockState x) (AbsBlockState y)
    = cmpX86State leq x y
  lub (AbsBlockState x) (AbsBlockState y)
    = AbsBlockState $ zipWithX86State lub x y

type AbsState = Map CodeAddr AbsBlockState

emptyAbsState :: AbsState
emptyAbsState = M.empty

data InterpState = InterpState
                   { _cfg      :: !CFG
                   , _failedAddrs  :: !(Set CodeAddr)
                   , _guessedAddrs :: !(Set CodeAddr)
                   , _blockEnds   :: !(Set CodeAddr)
                   , _genState :: !GlobalGenState
                   , _memory   :: !(Memory Word64) -- read only
                   , _absState :: !AbsState
                   }

emptyInterpState mem = InterpState
      { _cfg = emptyCFG
      , _failedAddrs = S.empty
      , _guessedAddrs = S.empty
      , _blockEnds   = S.empty
      , _genState    = emptyGlobalGenState
      , _memory      = mem
      , _absState    = emptyAbsState
      }

cfg :: Simple Lens InterpState CFG
cfg = lens _cfg (\s v -> s { _cfg = v })

genState :: Simple Lens InterpState GlobalGenState
genState = lens _genState (\s v -> s { _genState = v })

failedAddrs :: Simple Lens InterpState (Set CodeAddr)
failedAddrs = lens _failedAddrs (\s v -> s { _failedAddrs = v })

guessedAddrs :: Simple Lens InterpState (Set CodeAddr)
guessedAddrs = lens _guessedAddrs (\s v -> s { _guessedAddrs = v })

blockEnds :: Simple Lens InterpState (Set CodeAddr)
blockEnds = lens _blockEnds (\s v -> s { _blockEnds = v })

memory :: Simple Lens InterpState (Memory Word64)
memory = lens _memory (\s v -> s { _memory = v })

absState :: Simple Lens InterpState AbsState
absState = lens _absState (\s v -> s { _absState = v })

liftEither :: StateT s (Either e) a -> State s (Either e a)
liftEither m = state go
  where
    go s = case runStateT m s of
            Left e       -> (Left e,  s)
            Right (r, t) -> (Right r, t)

  -- FIXME: move
subMonad :: (MonadState s m) =>
            Simple Lens s t
            -> State t r
            -> m r
subMonad l m = l %%= runState m

------------------------------------------------------------------------
-- Block discovery

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock :: MonadState InterpState m => CodeAddr -> m (Maybe Block)
lookupBlock addr = uses (cfg . cfgBlocks) (M.lookup (DecompiledBlock addr))

lookupAbsState :: MonadState InterpState m => CodeAddr -> m (Maybe AbsBlockState)
lookupAbsState addr = uses absState (M.lookup addr)

newtype Hex = Hex Integer
              deriving (Eq, Ord)

mkHex :: Integer -> Hex
mkHex = Hex

instance Show Hex where
  show (Hex v) = showHex v ""

-- | This is the worker for getBlock, in the case that the cfg doesn't
-- contain the address of interest.
reallyGetBlock :: MonadState InterpState m => ExploreLoc -> m (Maybe Block)
reallyGetBlock loc = do
  mem <- use memory
  r <- subMonad genState (liftEither $ disassembleBlock mem loc)
  case r of
   Left _e -> trace ("Failed for address " ++ show (pretty loc)) $
              do failedAddrs %= S.insert (loc_ip loc)
                 return Nothing
   Right (bs, next_ip) ->
     do cfg       %= insertBlocksForCode (loc_ip loc) next_ip bs
        blockEnds %= S.insert next_ip
        lookupBlock (loc_ip loc)

-- | Returns a block at the given location, if at all possible.  This
-- will disassemble the binary if the block hasn't been seen before.
-- In particular, this ensures that a block and all it's children are
-- present in the cfg (assuming successful disassembly)
getBlock :: MonadState InterpState m => ExploreLoc -> m (Maybe Block)
getBlock loc = do let addr = (loc_ip loc)
                  m_b <- lookupBlock addr
                  failed <- uses failedAddrs (S.member addr)
                  case m_b of
                    Nothing | not failed -> reallyGetBlock loc
                    _                    -> return m_b

------------------------------------------------------------------------
-- Transfer functions

type Path = [(Value BoolType, Bool)]

-- blockPaths :: CFG -> Block -> [(X86State, Path)]
-- blockPaths c root = traverseBlockAndChildren c root go merge
--   where
--     merge cond l _ r = map (_2 %~ (:) (cond, True)) l
--                        ++ map (_2 %~ (:) (cond, False)) r
--     go b = case blockTerm b of
--             FetchAndExecute s -> [(s, [])]
--             _                 -> []

type AbsCache = MapF Assignment AbsValue

transferValue :: AbsBlockState
              -> AbsCache
              -> Value tp
              -> AbsValue tp
transferValue ab m v =
  case v of
   BVValue _ i -> abstractSingleton i
   -- Invariant: v is in m
   AssignedValue a ->
     fromMaybe (error $ "Missing assignment for " ++ show (assignId a))
               (MapF.lookup a m)
   Initial r -> unAbsValue' $ ab ^. (absX86State . register r)

transferApp :: AbsBlockState
            -> AbsCache
            -> App Value tp
            -> AbsValue tp
transferApp ab app m = top

type_width' :: TypeRepr tp -> Int
type_width' (BVTypeRepr n) = widthVal n

transferStmt :: AbsBlockState
             -> Stmt
             -> State AbsCache (Set CodeAddr)
transferStmt ab stmt = go stmt
  where
    go :: Stmt -> State AbsCache (Set CodeAddr)
    go (AssignStmt a@(Assignment v rhs)) =
      do modify (\m -> MapF.insert a (evalRHS m rhs) m)
         return S.empty
    go (Write (MemLoc _ tp) v)
      | type_width' tp == 64 = do
          vs <- gets (\s -> transferValue ab s v)
          return $ case concretize vs of
                    Nothing  -> S.empty
                    Just vs' -> S.map fromInteger vs'
    go _                               = return S.empty

    evalRHS :: forall tp
            .  AbsCache
            -> AssignRhs tp
            -> AbsValue tp
    evalRHS m rhs =
      case rhs of
       EvalApp app    -> transferApp ab m app
       SetUndefined _ -> TopV
       Read _         -> TopV

abstractState :: AbsBlockState
              -> AbsCache
              -> X86State
              -> [(ExploreLoc, AbsBlockState)]
abstractState ab m s =
  case concretize (unAbsValue' (abst ^. curIP)) of
   Nothing  -> trace "Hit top" [] -- we hit top, so give up
   Just ips ->
     [ (loc, AbsBlockState $ abst & curIP .~ x_w)
     | x <- S.toList ips
     , let x_w = AbsValue' (abstractSingleton x)
     , let t = case s ^. x87TopReg of
                BVValue _ v -> v
                _ -> error "x87top is not concrete"
     , let loc = ExploreLoc { loc_ip = fromInteger x
                            , loc_x87_top = fromInteger t
                            }
     ]
  where
    abst = mkX86State (\r -> AbsValue' $ transferValue ab m (s ^. register r))

transferBlock :: CFG -> Block -> AbsBlockState
                 -> (Set CodeAddr, [(ExploreLoc, AbsBlockState)])
transferBlock c root ab =
  traverseBlockAndChildren c root leaf merge MapF.empty
  & _1 %~ S.unions
  where
    merge b l r m = let (guesses, m_b) = go b m
                    in l m_b `mappend` r m_b `mappend` (guesses, [])
    leaf b m = case blockTerm b of
                 FetchAndExecute s ->
                   let (guesses, m_b) = go b m
                    in (guesses, abstractState ab m_b s)
                 _                 -> mempty -- can't happen

    go b = runState (mapM (transferStmt ab) (blockStmts b))


-- | Joins in the new abstract state and returns the locations for
-- which the new state is changed.
mergeBlocks :: [(ExploreLoc, AbsBlockState)] -> State AbsState [ExploreLoc]
mergeBlocks bs = state (\s -> Fold.foldl' mergeBlock ([], s) bs)
  where
    mergeBlock r@(locs, s) (loc, ab) =
      let upd new = ( loc : locs, M.insert (loc_ip loc) new s )
      in
      case M.lookup (loc_ip loc) s of
       -- We have seen this block before, so need to join and see if
       -- the results is changed.
       Just ab' -> case joinD ab' ab of
                    Nothing  -> r
                    Just new -> trace ("Merging state for " ++ show (pretty loc)
                                      ++ "\n" ++ show (pretty new))
                                $ upd new

       -- We haven't seen this block before
       Nothing  -> trace ("Adding state for " ++ show (pretty loc)
                          ++ "\n" ++ show (pretty ab)) $ upd ab

transfer :: MonadState InterpState m => ExploreLoc -> m [ExploreLoc]
transfer loc = do m_b  <- getBlock loc
                  m_ab <- lookupAbsState (loc_ip loc)
                  case (m_b, m_ab) of
                   (Just b, Just s) ->
                     do c <- use cfg
                        let (guesses, vs) = transferBlock c b s
                        guessedAddrs %= S.union guesses
                        subMonad absState (mergeBlocks vs)
                   _                -> return []

------------------------------------------------------------------------
-- Main loop

cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> (CFG, Set CodeAddr)
cfgFromAddress mem start = (s' ^. cfg, s' ^. blockEnds)
  where
    s' = go (emptyInterpState mem) (S.singleton start)
    go st roots
      | S.null roots = st
      | otherwise    =
          let wl   = WL.fromSet (S.map rootLoc roots)
              st'  = st & absState %~ addTops roots
              st'' = execState (WL.iterate transfer wl) st'
          in go (st'' & guessedAddrs .~ S.empty) (st'' ^. guessedAddrs)

    addTops roots m =
      Fold.foldl' (\m' k -> M.insertWith (\_ old -> old) k top m') m
      (S.filter isCodePointer roots)

    isCodePointer :: CodeAddr -> Bool
    isCodePointer val = addrHasPermissions (fromIntegral val) pf_x mem


------------------------------------------------------------------------
-- Pretty printing utilities (copied)

bracketsep :: [Doc] -> Doc
bracketsep [] = text "{}"
bracketsep (h:l) =
  vcat ([text "{" <+> h] ++ fmap (text "," <+>) l ++ [text "}"])

ppValueEq :: N.RegisterName cl -> AbsValue' (N.RegisterType cl) -> Maybe Doc
ppValueEq r v = pp <$> concretize (unAbsValue' v)
  where
    pp vs =  text (show r) <+> text "="
             <+> encloseSep lbrace rbrace comma (map (\v' -> text (showHex v' "")) ( S.toList vs))

-- | Pretty print  a register equals a value.
rec :: N.RegisterName cl -> AbsValue' (N.RegisterType cl) -> Maybe Doc
rec nm v = ppValueEq nm v

recv :: (Int -> N.RegisterName cl)
        -> V.Vector (AbsValue' (N.RegisterType cl)) -> [Maybe Doc]
recv mkR v = f <$> [0..V.length v - 1]
  where
    f i = ppValueEq (mkR i) (v V.! i)

parenIf :: Bool -> Doc -> Doc
parenIf True d = parens d
parenIf False d = d

instance Pretty AbsBlockState where
  pretty (AbsBlockState s) =
    bracketsep $ catMaybes ([ rec   N.rip (s^.curIP)]
                            ++ recv N.GPReg (s^.reg64Regs)
                            ++ recv N.FlagReg (s^.flagRegs)
                            ++ recv N.X87ControlReg (s^.x87ControlWord)
                            ++ recv N.X87StatusReg (s^.x87StatusWord)
                            ++ recv N.X87TagReg (s^.x87TagWords)
                            ++ recv N.X87FPUReg (s^.x87Regs)
                            ++ recv N.XMMReg (s^.xmmRegs))
