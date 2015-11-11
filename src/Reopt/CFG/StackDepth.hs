{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reopt.CFG.StackDepth
  ( maximumStackDepth
  , StackDepthValue(..)
  ) where

import           Control.Lens
import           Control.Monad (join)
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (toList, traverse_)
import           Data.Int
import           Data.Int (Int64)
import           Data.List (elemIndex, elem, partition, any)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Monoid (mconcat, mempty, Any(..))
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Type.Equality
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Maybe (catMaybes)

import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF

import           Reopt.Analysis.AbsState
import           Reopt.CFG.InterpState
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Object.Memory

import           Debug.Trace

data StackDepthOffset = Pos (Value (BVType 64)) | Neg (Value (BVType 64))
                      deriving (Eq, Ord, Show)

negateStackDepthOffset :: StackDepthOffset -> StackDepthOffset
negateStackDepthOffset (Pos x) = Neg x
negateStackDepthOffset (Neg x) = Pos x

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

isConstantDepthValue :: StackDepthValue -> Maybe Int64
isConstantDepthValue sv
  | Set.null (dynamicPart sv) = Just (staticPart sv)
  | otherwise                 = Nothing

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
  | dynamicPart v1 == dynamicPart v2 = staticPart v1 < staticPart v2
  -- FIXME: subsets etc.                                       
  | otherwise = False

-- could do this online, this helps with debugging though.
minimizeStackDepthValues :: Set StackDepthValue -> Set StackDepthValue
minimizeStackDepthValues = Set.fromList . Set.fold go []
  where
    -- FIXME: can we use ordering to simplify this?
    go v xs = let (subs, xs') = partition (subsumes v) xs
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
foldValue :: forall m tp. Monoid m 
             => (forall n.  NatRepr n -> Integer -> m)
             -> (forall cl. N.RegisterName cl -> m)
             -> Value tp -> m
foldValue litf initf val = go val
  where
    go :: forall tp. Value tp -> m
    go v = case v of
             BVValue sz i -> litf sz i
             Initial r    -> initf r 
             AssignedValue (Assignment _ rhs) -> goAssignRHS rhs

    goAssignRHS :: forall tp. AssignRhs tp -> m
    goAssignRHS v =
      case v of
        EvalApp a -> foldApp go a
        SetUndefined w -> mempty
        Read loc
         | MemLoc addr _ <- loc -> go addr
         | otherwise            -> mempty -- FIXME: what about ControlLoc etc.
        MemCmp _sz cnt src dest rev -> mconcat [ go cnt, go src, go dest, go rev ]

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
parseStackPointer' init addr
  -- assert sp occurs in at most once in either x and y  
  | Just (BVAdd _ x y) <- valueAsApp addr =
      addStackDepthValue (parseStackPointer' init x)
                         (parseStackPointer' init y)
 
  | Just (BVSub _ x y) <- valueAsApp addr =
      addStackDepthValue (parseStackPointer' init x)
                         (negateStackDepthValue (parseStackPointer' init y))      
  | BVValue _ i <- addr = constantDepthValue (fromIntegral i)
  | Initial n <- addr, Just Refl <- testEquality n N.rsp = init
  | otherwise = SDV 0 (Set.singleton (Pos addr))


-- FIXME: performance
parseStackPointer :: StackDepthValue -> Value (BVType 64) -> Set StackDepthValue
parseStackPointer init addr0
  | valueHasSP addr0 = Set.singleton (parseStackPointer' init addr0)
  | otherwise        = Set.empty

-- -----------------------------------------------------------------------------

isWriteTo :: Stmt -> Value (BVType 64) -> TypeRepr tp -> Maybe (Value tp)
isWriteTo (Write (MemLoc a _) val) expected tp
  | Just _ <- testEquality a expected
  , Just Refl <- testEquality (valueType val) tp =
    Just val
isWriteTo _ _ _ = Nothing

isCodeAddrWriteTo :: Memory Word64 -> Stmt -> Value (BVType 64) -> Maybe Word64
isCodeAddrWriteTo mem s sp
  | Just (BVValue _ val) <- isWriteTo s sp (knownType :: TypeRepr (BVType 64))
  , isCodeAddr mem (fromInteger val)
  = Just (fromInteger val)
isCodeAddrWriteTo _ _ _ = Nothing

identifyCall :: Memory Word64
             -> [Stmt]
             -> X86State Value
             -> Maybe (Seq Stmt, Word64, [Some Value])
identifyCall mem stmts0 s = go (Seq.fromList stmts0)
  where next_sp = s^.register N.rsp
        go stmts =
          case Seq.viewr stmts of
            Seq.EmptyR -> Nothing
            prev Seq.:> stmt
              | Just ret <- isCodeAddrWriteTo mem stmt next_sp ->
                Just (prev, ret, [])
              | Write{} <- stmt -> Nothing
              | otherwise -> go prev

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a X86State to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
identifyReturn :: X86State Value -> Maybe (Assignment (BVType 64))
identifyReturn s = do
  let next_ip = s^.register N.rip
      next_sp = s^.register N.rsp
  case next_ip of
    AssignedValue assign@(Assignment _ (Read (MemLoc ip_addr _)))
      | let (ip_base, ip_off) = asBaseOffset ip_addr
      , let (sp_base, sp_off) = asBaseOffset next_sp
      , (ip_base, ip_off + 8) == (sp_base, sp_off) -> Just assign
    _ -> Nothing

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
      Just b       <- return $ lookupBlock (interp_state ^. blocks) lbl

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
          
      
      case blockTerm b of
        Branch _c x y -> do
          traverse_ goStmt (blockStmts b)
          go init_sp x 
          go init_sp y

        FetchAndExecute proc_state
          -- The last statement was a call.
          | Just (stmts', ret_addr, _) <- identifyCall (memory interp_state) (blockStmts b) proc_state -> do

            traverse_ goStmt stmts'
            addStateVars proc_state

            let lbl' = GeneratedBlock ret_addr 0
                sp'  = parseStackPointer' init_sp (proc_state ^. register N.rsp)
            addBlock lbl' (addStackDepthValue sp' $ constantDepthValue 8)
     
          -- Jump to concrete offset.
          | BVValue _ (fromInteger -> tgt_addr) <- proc_state^.register N.rip
            -- Check that we are in the same function
          , inSameFunction (labelAddr lbl) tgt_addr interp_state -> do

            traverse_ goStmt (blockStmts b)
            addStateVars proc_state
            
            let lbl'     = GeneratedBlock tgt_addr 0
                sp' = parseStackPointer' init_sp (proc_state ^. register N.rsp)
            
            addBlock lbl' sp'
     
           -- Return
          | Just assign <- identifyReturn proc_state -> traverse_ goStmt (blockStmts b)

        _ -> return () -- ???


