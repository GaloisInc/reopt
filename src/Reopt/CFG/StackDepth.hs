{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reopt.CFG.StackDepth
  ( maximumStackDepth
  , StackDepthValue(..)
  , StackDepthOffset(..)
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (traverse_)
import           Data.Int
import           Data.List (partition)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Any(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Type.Equality
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Reopt.CFG.InterpState
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types

import           Debug.Trace

data StackDepthOffset = Pos (Value (BVType 64)) | Neg (Value (BVType 64))
                      deriving (Eq, Ord, Show)

negateStackDepthOffset :: StackDepthOffset -> StackDepthOffset
negateStackDepthOffset (Pos x) = Neg x
negateStackDepthOffset (Neg x) = Pos x

isNegativeDepth :: StackDepthOffset -> Bool
isNegativeDepth (Neg _) = True
isNegativeDepth _ = False

-- One stack expression, basically staticPart + \Sigma dynamicPart
data StackDepthValue = SDV { staticPart :: !Int64, dynamicPart :: !(Set StackDepthOffset) } 
                       deriving (Eq, Ord, Show)

instance Pretty StackDepthValue where
  pretty sdv = integer (fromIntegral $ staticPart sdv)
               <+> go (Set.toList $ dynamicPart sdv)
    where
      go []           = mempty
      go (Pos x : xs) = text "+" <+> pretty x <+> go xs
      go (Neg x : xs) = text "-" <+> pretty x <+> go xs

-- isConstantDepthValue :: StackDepthValue -> Maybe Int64
-- isConstantDepthValue sv
--   | Set.null (dynamicPart sv) = Just (staticPart sv)
--   | otherwise                 = Nothing

constantDepthValue :: Int64 -> StackDepthValue
constantDepthValue c = SDV c Set.empty

addStackDepthValue :: StackDepthValue -> StackDepthValue -> StackDepthValue
addStackDepthValue sdv1 sdv2  = SDV (staticPart sdv1 + staticPart sdv2)
                                    (dynamicPart sdv1 `Set.union` dynamicPart sdv2)

negateStackDepthValue :: StackDepthValue -> StackDepthValue
negateStackDepthValue sdv = SDV { staticPart  = - (staticPart sdv)
                                , dynamicPart = Set.map negateStackDepthOffset (dynamicPart sdv) }

-- | v1 `subsumes` v2 if a stack of depth v1 is always larger than a
-- stack of depth v2.  Note that we are interested in negative values
-- primarily.
subsumes :: StackDepthValue -> StackDepthValue -> Bool
subsumes v1 v2
  | dynamicPart v2 `Set.isSubsetOf` dynamicPart v1 = staticPart v1 <= staticPart v2
  -- FIXME: subsets etc.                                       
  | otherwise = False

-- could do this online, this helps with debugging though.
minimizeStackDepthValues :: Set StackDepthValue -> Set StackDepthValue
minimizeStackDepthValues = Set.fromList . Set.fold go [] . Set.map discardPositive
  where
    discardPositive v = v { dynamicPart = Set.filter isNegativeDepth (dynamicPart v) }
    -- FIXME: can we use ordering to simplify this?
    go v xs = let (_subs, xs') = partition (subsumes v) xs
                  dominated   = any (`subsumes` v) xs'
              in if not dominated then v : xs' else xs'
      
-- -----------------------------------------------------------------------------

-- For now we assume that the stack pointer a the start of a block is path-independent
type BlockStackStart = StackDepthValue

-- For now this is just the set of stack addresses referenced by the
-- program --- note that as it is partially symbolic, we can't always
-- statically determine the stack depth (might depend on arguments, for example).
type BlockStackDepths = Set StackDepthValue

-- We use BlockLabel but only really need CodeAddr (sub-blocks shouldn't appear)
data StackDepthState = SDS { _blockInitStackPointers :: !(Map BlockLabel BlockStackStart) 
                           , _blockStackRefs :: !(BlockStackDepths)
                           , _blockFrontier  :: !(Set BlockLabel)  }

blockInitStackPointers :: Simple Lens StackDepthState (Map BlockLabel BlockStackStart)
blockInitStackPointers = lens _blockInitStackPointers (\s v -> s { _blockInitStackPointers = v })

blockStackRefs :: Simple Lens StackDepthState (BlockStackDepths)
blockStackRefs = lens _blockStackRefs (\s v -> s { _blockStackRefs = v })

blockFrontier :: Simple Lens StackDepthState (Set BlockLabel)
blockFrontier = lens _blockFrontier (\s v -> s { _blockFrontier = v })

-- ----------------------------------------------------------------------------------------

-- FIXME: move

-- Unwraps all Apps etc, might visit an app twice (Add x x, for example)
-- foldValue :: forall m tp. Monoid m 
--              => (forall n.  NatRepr n -> Integer -> m)
--              -> (forall cl. N.RegisterName cl -> m)
--              -> Value tp -> m
-- foldValue litf initf val = go val
--   where
--     go :: forall tp. Value tp -> m
--     go v = case v of
--              BVValue sz i -> litf sz i
--              Initial r    -> initf r 
--              AssignedValue (Assignment _ rhs) -> goAssignRHS rhs

--     goAssignRHS :: forall tp. AssignRhs tp -> m
--     goAssignRHS v =
--       case v of
--         EvalApp a -> foldApp go a
--         SetUndefined w -> mempty
--         Read loc
--          | MemLoc addr _ <- loc -> go addr
--          | otherwise            -> mempty -- FIXME: what about ControlLoc etc.
--         MemCmp _sz cnt src dest rev -> mconcat [ go cnt, go src, go dest, go rev ]

-- ----------------------------------------------------------------------------------------

type StackDepthM a = State StackDepthState a

addBlock :: BlockLabel -> BlockStackStart -> StackDepthM ()
addBlock lbl start =
  do x <- use (blockInitStackPointers . at lbl)
     case x of
       Nothing     -> do blockInitStackPointers %= Map.insert lbl start
                         blockFrontier %= Set.insert lbl
       Just start'
        | start == start' -> return ()
        | otherwise       -> traceM ("Block stack depth mismatch at " ++ show (pretty lbl) ++ ": " ++ (show (pretty start)) ++ " and " ++ (show (pretty start'))) 

nextBlock :: StackDepthM (Maybe BlockLabel)
nextBlock = blockFrontier %%= \s -> let x = Set.maxView s in (fmap fst x, maybe s snd x)

addDepth :: Set StackDepthValue -> StackDepthM ()
addDepth v = blockStackRefs %= Set.union v

valueHasSP :: Value (BVType 64) -> Bool
valueHasSP val = go val
  where
    go :: forall tp. Value tp -> Bool
    go v = case v of
             BVValue _sz _i -> False
             Initial r      -> testEquality r N.rsp /= Nothing
             AssignedValue (Assignment _ rhs) -> goAssignRHS rhs

    goAssignRHS :: forall tp. AssignRhs tp -> Bool
    goAssignRHS v =
      case v of
        EvalApp a -> getAny $ foldApp (Any . go)  a
        MemCmp _sz cnt src dest rev -> or [ go cnt, go src, go dest, go rev ]
        _ -> False

parseStackPointer' :: StackDepthValue -> Value (BVType 64) -> StackDepthValue
parseStackPointer' sp0 addr
  -- assert sp occurs in at most once in either x and y  
  | Just (BVAdd _ x y) <- valueAsApp addr =
      addStackDepthValue (parseStackPointer' sp0 x)
                         (parseStackPointer' sp0 y)
 
  | Just (BVSub _ x y) <- valueAsApp addr =
      addStackDepthValue (parseStackPointer' sp0 x)
                         (negateStackDepthValue (parseStackPointer' sp0 y))      
  | BVValue _ i <- addr = constantDepthValue (fromIntegral i)
  | Initial n <- addr, Just Refl <- testEquality n N.rsp = sp0
  | otherwise = SDV 0 (Set.singleton (Pos addr))


-- FIXME: performance
parseStackPointer :: StackDepthValue -> Value (BVType 64) -> Set StackDepthValue
parseStackPointer sp0 addr0
  | valueHasSP addr0 = Set.singleton (parseStackPointer' sp0 addr0)
  | otherwise        = Set.empty

-- -----------------------------------------------------------------------------

-- | Returns the maximum stack argument used by the function, that is,
-- the highest index above sp0 that is read or written.
maximumStackDepth :: InterpState -> CodeAddr -> BlockStackDepths
maximumStackDepth ist addr =
  minimizeStackDepthValues $ (^. blockStackRefs) $ execState (recoverIter ist Set.empty (Just lbl0)) s0                         
  where
    s0   = SDS { _blockInitStackPointers = Map.singleton lbl0 sdv0
               , _blockStackRefs         = Set.empty
               , _blockFrontier          = Set.empty }
    lbl0 = GeneratedBlock addr 0
    sdv0 = SDV { staticPart = 0, dynamicPart = Set.empty }

-- | Explore states until we have reached end of frontier.
recoverIter :: InterpState
               -> Set BlockLabel
               -> Maybe BlockLabel
               -> StackDepthM ()
recoverIter _   _     Nothing = return ()
recoverIter ist seen (Just lbl)
  | lbl `Set.member` seen = nextBlock >>= recoverIter ist seen
  | otherwise = do recoverBlock ist lbl
                   lbl' <- nextBlock
                   recoverIter ist (Set.insert lbl seen) lbl'

recoverBlock :: InterpState 
                -> BlockLabel
                -> StackDepthM ()
recoverBlock interp_state root_label = do
  Just init_sp <- use (blockInitStackPointers . at root_label)
  go init_sp root_label
  where
    go init_sp lbl = do
      Just (b, m_pterm) <- return $ getClassifyBlock lbl interp_state

      let goStmt (AssignStmt (Assignment _ (Read (MemLoc addr _))))
            = addDepth $ parseStackPointer init_sp addr
          goStmt (Write (MemLoc addr tp) v) 
            = do addDepth $ parseStackPointer init_sp addr
                 case testEquality tp (knownType :: TypeRepr (BVType 64)) of 
                   Just Refl -> addDepth $ parseStackPointer init_sp v
                   _ -> return ()

          goStmt _ = return ()

          -- overapproximates by viewing all registers as uses of the
          -- sp between blocks
          addStateVars s = 
            addDepth $ Set.unions [ parseStackPointer init_sp (s ^. register r)
                                  | r <- N.gpRegs ]
          
      case m_pterm of
        Just (ParsedBranch _c x y) -> do
          traverse_ goStmt (blockStmts b)
          go init_sp x 
          go init_sp y

        Just (ParsedCall proc_state stmts' _fn m_ret_addr) -> do 
            traverse_ goStmt stmts'
            addStateVars proc_state

            let sp'  = parseStackPointer' init_sp (proc_state ^. register N.rsp)
            case m_ret_addr of
              Nothing -> return ()
              Just ret_addr ->  addBlock (mkRootBlockLabel ret_addr) (addStackDepthValue sp' $ constantDepthValue 8)

        Just (ParsedJump proc_state tgt_addr) -> do 
            traverse_ goStmt (blockStmts b)
            addStateVars proc_state
            
            let lbl'     = mkRootBlockLabel tgt_addr
                sp' = parseStackPointer' init_sp (proc_state ^. register N.rsp)
            
            addBlock lbl' sp'

        Just (ParsedReturn _proc_state stmts') -> do
            traverse_ goStmt stmts'

        Just (ParsedLookupTable _proc_state _idx _vec) -> error "LookupTable"

        Nothing -> return () -- ???


