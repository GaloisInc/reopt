{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.AbsState
  ( AbsBlockState
  , absX86State
  , absBlockDiff
  , AbsValue(..)
  , ppAbsValue
  , abstractSingleton
  , concreteStackOffset
  , concretize
  , asConcreteSingleton
  , meet
  , AbsDomain(..)
  , AbsRegs
  , absInitialRegs
  , initAbsRegs
  , absAssignments
  , assignLens
  , finalAbsBlockState
  , addAssignment
  , addMemWrite
  , transferValue
  , transferRHS
  , abstractLt
  , abstractLeq
  ) where

import           Control.Applicative ( (<$>) )
import           Control.Exception (assert)
import           Control.Lens
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Reopt.Domains.StridedInterval as SI
import           Reopt.Semantics.Representation
import qualified Reopt.Semantics.StateNames as N
import           Reopt.Semantics.Types

import           Debug.Trace

------------------------------------------------------------------------
-- Abstract states

class Eq d => AbsDomain d where
  -- | The top element
  top :: d

  -- | A partial ordering over d.  forall x. x `leq` top
  leq :: d -> d -> Bool
  leq x y =
    case joinD y x of
      Nothing -> True
      Just _ -> False

  -- | Least upper bound (always defined, as we have top)
  lub :: d -> d -> d
  lub x y = case joinD x y of
              Nothing -> x
              Just r -> r

  -- | Join the old and new states and return the updated state iff
  -- the result is larger than the old state.
  joinD :: d -> d -> Maybe d
  joinD old new
    | new `leq` old = Nothing
    | otherwise     = Just $ lub old new

  {-# MINIMAL (top, ((leq,lub) | joinD)) #-}

type ValueSet = Set Integer

------------------------------------------------------------------------
-- AbsValue

data AbsValue (tp :: Type) where
  -- | An absolute value.
  AbsValue :: !ValueSet -> AbsValue (BVType n)
  -- | Offset of stack at beginning of the block.
  StackOffset :: !ValueSet -> AbsValue (BVType 64)
  -- | An address (doesn't constraint value precisely).
  SomeStackOffset :: AbsValue (BVType 64)
  -- | A strided interval
  StridedInterval :: !(SI.StridedInterval (BVType n)) -> AbsValue (BVType n)
  -- | A sub-value about which we know more than the containing value
  -- (e.g., we know that the lower 8 bits are < 10)

  -- FIXME: we probably want n < n'
  SubValue :: !(NatRepr n) -> !(AbsValue (BVType n))
              -> !(AbsValue (BVType n')) -> AbsValue (BVType n')
  -- | Any value
  TopV :: AbsValue tp

prop_SubValue :: AbsValue tp -> Bool
prop_SubValue (SubValue n v v') =
  case (concretize v, concretize v') of
   (_, Nothing)         -> True
   -- FIXME: maybe? this just says all lower bits are set.  We could
   -- expand out the Top, but that might be expensive for large n.
   (Nothing, _)         -> False 
   (Just vs, Just vs')  -> vs `Set.isSubsetOf` (Set.map (toUnsigned n) vs')
prop_SubValue _ = True

-- | The maximum number of values we hold in a ValueSet, after which
-- we move to intervals
maxSetSize :: Int
maxSetSize = 5

-- Note that this is syntactic equality only.
instance Eq (AbsValue tp) where
  AbsValue x    == AbsValue y    = x == y
  StackOffset s == StackOffset t = s == t
  SomeStackOffset          == SomeStackOffset = True
  StridedInterval si1 == StridedInterval si2  = si1 == si2
  SubValue n v c == SubValue n' v' c'
    | Just Refl <- testEquality n n' = v == v' && c == c'
    | otherwise = False
  TopV          == TopV          = True
  _             ==               _ = False

instance EqF AbsValue where
  eqF = (==)

instance Pretty (AbsValue tp) where
  pretty (AbsValue s) = ppIntegerSet s
  pretty (StridedInterval s) = pretty s
  pretty (StackOffset s) = text "rsp_0 +" <+> ppIntegerSet s
  pretty SomeStackOffset = text "rsp_0 + ?"
  pretty (SubValue n v c) = pretty c <+> parens (int (natValue n) <+> pretty v)
  pretty TopV = text "top"

ppIntegerSet :: (Show w, Integral w) => Set w -> Doc
ppIntegerSet vs = encloseSep lbrace rbrace comma (map ppv (Set.toList vs))
  where ppv v' = assert (v' >= 0) $ text ("0x" ++ showHex v' "")


-- | Returns a set of concrete integers that this value may be.
-- This function will neither return the complete set or an
-- known under-approximation.
concretize :: AbsValue tp -> Maybe (Set Integer)
concretize (AbsValue s) = Just s
concretize (StridedInterval s) = Just (Set.fromList (SI.toList s))
-- OPT: might be faster to just do concretize v'
concretize (SubValue n v v') =
  do v_vs <- concretize v
     v_vs' <- concretize v'
     return (Set.filter (flip Set.member v_vs . toUnsigned n) v_vs')
concretize _ = Nothing

-- | Return single value is the abstract value can only take on one value.
asConcreteSingleton :: AbsValue tp -> Maybe Integer
asConcreteSingleton v =
  case Set.toList <$> concretize v of
    Just [e] -> Just e
    _ -> Nothing

-- -----------------------------------------------------------------------------
-- Smart constructors

-- | Smart constructor for strided intervals which takes care of top
stridedInterval :: SI.StridedInterval (BVType tp) -> AbsValue (BVType tp)
stridedInterval si
  | SI.isTop si = TopV
  | otherwise   = StridedInterval si

-- | Smart constructor for sub-values.  This ensures that the
-- subvalues are sorted on size.
subValue :: NatRepr n -> AbsValue (BVType n) -> AbsValue tp -> AbsValue tp
subValue n v v'@(SubValue nc vc container)
  -- FIXME: meet seems like what we want here? Maybe the combination
  -- operator should be a parameter (i.e. maybe join is what we want)
  | Just Refl <- testEquality n nc = SubValue nc (meet v vc) container 
  | Just LeqProof <- testLeq n nc  = SubValue n v v'
  | otherwise = SubValue nc vc (subValue n v container)

-- -----------------------------------------------------------------------------
-- Instances

instance AbsDomain (AbsValue tp) where
  top = TopV

{-
  leq _ TopV = True
  leq TopV _ = False
  leq (StackOffset s) (StackOffset t) = s `Set.isSubsetOf` t
  leq (AbsValue v) (AbsValue v') = v `Set.isSubsetOf` v'
  leq _ _ = False

  lub (StackOffset s) (StackOffset t) = StackOffset $ s `Set.union` t
  lub (AbsValue v) (AbsValue v') = AbsValue $ v `Set.union` v'
  lub _ _ = TopV
-}

  -- | Join the old and new states and return the updated state iff
  -- the result is larger than the old state.
  joinD TopV _ = Nothing
  joinD (AbsValue old) (AbsValue new)
      | new `Set.isSubsetOf` old = Nothing
      | Set.size r > maxSetSize = Just TopV
      | otherwise = Just (AbsValue r)
    where r = Set.union old new
  joinD (StackOffset old) (StackOffset new)
      | new `Set.isSubsetOf` old = Nothing
      | Set.size r > maxSetSize = Just TopV
      | otherwise = Just (StackOffset r)
    where r = Set.union old new

  -- Intervals
  joinD v v'
    | StridedInterval si_old <- v, StridedInterval si_new <- v'
    , si_new `SI.isSubsetOf` si_old = Nothing
    | StridedInterval si_old <- v, StridedInterval si_new <- v'
      = go si_old si_new
    | StridedInterval si <- v,  AbsValue s <- v'
      = go si (SI.fromFoldable (type_width (SI.typ si)) s)
    | StridedInterval si <- v', AbsValue s <- v 
      = go si (SI.fromFoldable (type_width (SI.typ si)) s)
    where go si1 si2 = Just $ stridedInterval (SI.lub si1 si2)

  -- Sub values

  -- FIXME: can we do something more fine-grained here than just
  -- dropping the sub-value?
  joinD v v'
    | SubValue n av c <- v, SubValue n' av' c' <- v'
    , Just Refl <- testEquality n n' =
      case (joinD av av', joinD c c') of
       (Nothing, Nothing)     -> Nothing
       (new_av, new_c) ->
         Just $ SubValue n (fromMaybe av new_av) (fromMaybe c new_c)           
      -- if n < n' (or n' < n) we drop the sub-value and always return
      -- a new value
    | SubValue n av c <- v, SubValue n' av' c' <- v'
    , Just LeqProof <- testLeq n n' = Just $ fromMaybe c (joinD c v')
    | SubValue n av c <- v, SubValue n' av' c' <- v' =
        Just $ fromMaybe v (joinD v c')
    | SubValue _n _av c <- v  = Just $ fromMaybe c (joinD c v')
    | SubValue _n _av c <- v' = Just $ fromMaybe v (joinD v c)
  
  -- Join addresses
  joinD SomeStackOffset StackOffset{} = Nothing
  joinD StackOffset{} SomeStackOffset = Just SomeStackOffset
  joinD SomeStackOffset SomeStackOffset = Nothing

  joinD _ _ = Just TopV

meet :: AbsValue tp -> AbsValue tp -> AbsValue tp
meet TopV x = x
meet x TopV = x
-- FIXME: reuse an old value if possible?
meet (AbsValue old) (AbsValue new) = AbsValue $ Set.intersection old new
meet (StackOffset old) (StackOffset new) =
  StackOffset $ Set.intersection old new

-- Intervals
meet v v'
  | StridedInterval si_old <- v, StridedInterval si_new <- v'
    = stridedInterval $ si_old `SI.glb` si_new
  | StridedInterval si <- v,  AbsValue s <- v'
    = AbsValue $ Set.filter (`SI.member` si) s
  | StridedInterval si <- v', AbsValue s <- v
    = AbsValue $ Set.filter (`SI.member` si) s

meet v v'
  | SubValue n av c <- v, SubValue n' av' c' <- v'
  , Just Refl <- testEquality n n' =
    SubValue n (meet av av') (meet c c')

  | SubValue n av c <- v, SubValue n' av' c' <- v'
  , Just LeqProof <- testLeq n n' = error "FIXME"
                                    
  | SubValue n av c <- v, SubValue n' av' c' <- v' =
      Just $ fromMaybe v (joinD v c')
  | SubValue _n _av c <- v  = Just $ fromMaybe c (joinD c v')
  | SubValue _n _av c <- v' = Just $ fromMaybe v (joinD v c)

-- Join addresses
meet SomeStackOffset s@StackOffset{} = s
meet s@StackOffset{} SomeStackOffset = s
meet SomeStackOffset SomeStackOffset = SomeStackOffset
meet _ _ = error "meet: impossible"

trunc :: (v+1 <= u)
      => AbsValue (BVType u)
      -> NatRepr v
      -> AbsValue (BVType v)
trunc (AbsValue s) w = AbsValue (Set.map (toUnsigned w) s)
trunc (StridedInterval s) w = stridedInterval (SI.trunc s w)
trunc (StackOffset _) _ = TopV
trunc SomeStackOffset _ = TopV
trunc TopV _ = TopV

uext :: (u+1 <= v) => AbsValue (BVType u) -> NatRepr v -> AbsValue (BVType v)
uext (AbsValue s) _ = AbsValue s
uext (StridedInterval si) w = StridedInterval (si { SI.typ = BVTypeRepr w } ) -- FIXME
uext (StackOffset _) _ = TopV
uext SomeStackOffset _ = TopV
uext TopV _ = TopV

bvadd :: NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
-- Stacks      
bvadd w (StackOffset s) (AbsValue t) | [o] <- Set.toList t = do
  StackOffset $ Set.map (addOff w o) s  
bvadd w (AbsValue t) (StackOffset s) | [o] <- Set.toList t = do
  StackOffset $ Set.map (addOff w o) s
-- Strided intervals
bvadd w v v'
  | StridedInterval si1 <- v, StridedInterval si2 <- v' = go si1 si2
  | StridedInterval si <- v,  AbsValue s <- v' = go si (SI.fromFoldable w s)
  | StridedInterval si <- v', AbsValue s <- v  = go si (SI.fromFoldable w s)
  where
    go si1 si2 = stridedInterval $ SI.bvadd w si1 si2
    
-- the rest  
bvadd _ StackOffset{} _ = SomeStackOffset
bvadd _ _ StackOffset{} = SomeStackOffset
bvadd _ SomeStackOffset _ = SomeStackOffset
bvadd _ _ SomeStackOffset = SomeStackOffset
bvadd _ _ _ = TopV

setL :: ([Integer] -> AbsValue (BVType n))
     -> (Set Integer -> AbsValue (BVType n))
     -> [Integer]
     -> AbsValue (BVType n)
setL def c l | length l > maxSetSize = def l
             | otherwise = c (Set.fromList l)

bvsub :: NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
bvsub w (AbsValue s) (AbsValue t) =
  setL (stridedInterval . SI.fromFoldable w) AbsValue $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x - y))
bvsub w v v'
  | StridedInterval si1 <- v, StridedInterval si2 <- v' = go si1 si2
  | StridedInterval si <- v,  AbsValue s <- v' = go si (SI.fromFoldable w s)
  | StridedInterval si <- v', AbsValue s <- v  = go si (SI.fromFoldable w s)
  where
    go _si1 _si2 = TopV -- FIXME
bvsub w (StackOffset s) (AbsValue t) = setL (const SomeStackOffset) StackOffset $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x - y))
bvsub _ StackOffset{} _ = SomeStackOffset
bvsub _ _ StackOffset{} = TopV
bvsub _ SomeStackOffset _ = SomeStackOffset
bvsub _ _ SomeStackOffset = TopV
bvsub _ _ _ = TopV -- Keep the pattern checker happy
-- bvsub _ TopV _ = TopV
-- bvsub _ _ TopV = TopV

bvmul :: NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
bvmul w (AbsValue s) (AbsValue t) =
  setL (stridedInterval . SI.fromFoldable w) AbsValue $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x * y))
bvmul w v v'
  | StridedInterval si1 <- v, StridedInterval si2 <- v' = go si1 si2
  | StridedInterval si <- v,  AbsValue s <- v' = go si (SI.fromFoldable w s)
  | StridedInterval si <- v', AbsValue s <- v  = go si (SI.fromFoldable w s)
  where
    go si1 si2 = stridedInterval $ SI.bvmul w si1 si2    
bvmul _ _ _ = TopV

ppAbsValue :: AbsValue tp -> Maybe Doc
ppAbsValue TopV = Nothing
ppAbsValue v = Just (pretty v)

-- | Print a list of Docs vertically separated.
instance PrettyRegValue AbsValue where
  ppValueEq _ TopV = Nothing
  ppValueEq r v = Just (text (show r) <+> text "=" <+> pretty v)

abstractSingleton :: NatRepr n -> Integer -> AbsValue (BVType n)
abstractSingleton n i
  | 0 <= i && i <= maxUnsigned n = AbsValue (Set.singleton i)
  | otherwise = error $ "abstractSingleton given bad value: " ++ show i ++ " " ++ show n

concreteStackOffset :: Integer -> AbsValue (BVType 64)
concreteStackOffset o = StackOffset (Set.singleton o)

------------------------------------------------------------------------
-- Restrictions

hasMaximum :: TypeRepr tp -> AbsValue tp -> Maybe Integer
hasMaximum tp v =
  case v of
   AbsValue s         -> Just (Set.findMax s)
   StridedInterval si -> Just (SI.intervalEnd si)
   TopV               -> Just $ case tp of BVTypeRepr n -> maxUnsigned n
   _                  -> Nothing


hasMinimum :: TypeRepr tp -> AbsValue tp -> Maybe Integer
hasMinimum tp v =
  case v of
   AbsValue s         -> Just (Set.findMin s)
   StridedInterval si -> Just (SI.base si)
   TopV               -> Just 0
   _                  -> Nothing

-- | @abstractLt x y@ refines x and y with the knowledge that @x < y@
--
-- For example, given {2, 3} and {2, 3, 4}, we know (only) that
-- {2, 3} and {3, 4} because we may pick any element from either set.

abstractLt :: TypeRepr tp
              -> AbsValue tp -> AbsValue tp
              -> (AbsValue tp, AbsValue tp)
abstractLt _tp TopV TopV = (TopV, TopV)              
abstractLt tp x y
  | Just u_y <- hasMaximum tp y 
  , Just l_x <- hasMinimum tp x
  , BVTypeRepr n <- tp =
    -- trace ("abstractLt " ++ show (pretty x) ++ " " ++ show (pretty y))    
    ( meet x (stridedInterval $ SI.mkStridedInterval tp False 0 (u_y - 1) 1)
    , meet y (stridedInterval $ SI.mkStridedInterval tp False (l_x + 1)
                                                     (maxUnsigned n) 1))
abstractLt _tp x y = (x, y)

-- | @abstractLeq x y@ refines x and y with the knowledge that @x <= y@
abstractLeq :: TypeRepr tp
               -> AbsValue tp -> AbsValue tp
               -> (AbsValue tp, AbsValue tp)
abstractLeq _tp TopV TopV = (TopV, TopV)
abstractLeq tp x y
  | Just u_y <- hasMaximum tp y 
  , Just l_x <- hasMinimum tp x
  , BVTypeRepr n <- tp =
    -- trace ("abstractLeq " ++ show (pretty x) ++ " " ++ show (pretty y))    
    ( meet x (stridedInterval $ SI.mkStridedInterval tp False 0 u_y 1)
    , meet y (stridedInterval $ SI.mkStridedInterval tp False l_x
                                                     (maxUnsigned n) 1))
abstractLeq _tp x y = (x, y)

------------------------------------------------------------------------
-- AbsBlockState

data StackEntry where
  StackEntry :: TypeRepr tp -> AbsValue tp -> StackEntry

instance Eq StackEntry where
  StackEntry x_tp x_v == StackEntry y_tp y_v
    | Just Refl <- testEquality x_tp y_tp = x_v == y_v
    | otherwise = False

type AbsBlockStack = Map Integer StackEntry

absStackLeq :: AbsBlockStack -> AbsBlockStack -> Bool
absStackLeq x y = all entryLeq (Map.toList y)
  where entryLeq (o, StackEntry y_tp y_v) =
          case Map.lookup o x of
            Just (StackEntry x_tp x_v) | Just Refl <- testEquality x_tp y_tp ->
              leq x_v y_v
            _ -> False

absStackLub :: AbsBlockStack -> AbsBlockStack -> AbsBlockStack
absStackLub = Map.mergeWithKey merge (\_ -> Map.empty) (\_ -> Map.empty)
  where merge :: Integer -> StackEntry -> StackEntry -> Maybe StackEntry
        merge _ (StackEntry x_tp x_v) (StackEntry y_tp y_v) =
          case testEquality x_tp y_tp of
            Just Refl ->
              case lub x_v y_v of
                TopV -> Nothing
                v -> Just (StackEntry x_tp v)
            Nothing -> Nothing

ppAbsStack :: AbsBlockStack -> Doc
ppAbsStack m = vcat (pp <$> Map.toList m)
  where pp (o,StackEntry _ v) = text (show o) <+> text ":=" <+> pretty v

-- | State at beginning of a block.
data AbsBlockState
      = AbsBlockState { _absX86State :: !(X86State AbsValue)
                      , _startAbsStack :: !AbsBlockStack
                      }
  deriving Eq


absX86State :: Simple Lens AbsBlockState (X86State AbsValue)
absX86State = lens _absX86State (\s v -> s { _absX86State = v })

startAbsStack :: Simple Lens AbsBlockState AbsBlockStack
startAbsStack = lens _startAbsStack (\s v -> s { _startAbsStack = v })


instance AbsDomain AbsBlockState where
  top = AbsBlockState { _absX86State = mkX86State (\_ -> top)
                      , _startAbsStack = Map.empty
                      }

  leq x y =
    cmpX86State leq (x^.absX86State) (y^.absX86State)
      && absStackLeq (x^.startAbsStack) (y^.startAbsStack)

  lub x y =
    AbsBlockState { _absX86State   = zipWithX86State lub (x^.absX86State) (y^.absX86State)
                  , _startAbsStack = absStackLub (x^.startAbsStack) (y^.startAbsStack)
                  }

instance Pretty AbsBlockState where
  pretty s =
      text "registers:" <$$>
      indent 2 (pretty (s^.absX86State)) <$$>
      stack_d
    where stack = s^.startAbsStack
          stack_d | Map.null stack = empty
                  | otherwise = text "stack:" <$$>
                                indent 2 (ppAbsStack (s^.startAbsStack))

instance Show AbsBlockState where
  show s = show (pretty s)


absBlockDiff :: AbsBlockState -> AbsBlockState -> [Some N.RegisterName]
absBlockDiff x y = filter isDifferent x86StateRegisters
  where isDifferent (Some n) = x^.absX86State^.register n /= y^.absX86State^.register n

------------------------------------------------------------------------
-- AbsRegs

-- | This is used to cache all changes to a state within a block.
data AbsRegs = AbsRegs { _absInitialRegs :: !(X86State AbsValue)
                       , _absAssignments :: !(MapF Assignment AbsValue)
                       , _curAbsStack :: !AbsBlockStack
                       }

-- FIXME
instance Pretty AbsRegs where
  pretty regs = pretty (AbsBlockState { _absX86State   = regs ^. absInitialRegs 
                                      , _startAbsStack = regs ^. curAbsStack })
 

                        
initAbsRegs :: AbsBlockState -> AbsRegs
initAbsRegs s = AbsRegs { _absInitialRegs = s^.absX86State
                        , _absAssignments = MapF.empty
                        , _curAbsStack = s^.startAbsStack
                        }


absInitialRegs :: Simple Lens AbsRegs (X86State AbsValue)
absInitialRegs = lens _absInitialRegs (\s v -> s { _absInitialRegs = v })

absAssignments :: Simple Lens AbsRegs (MapF Assignment AbsValue)
absAssignments = lens _absAssignments (\s v -> s { _absAssignments = v })

curAbsStack :: Simple Lens AbsRegs AbsBlockStack
curAbsStack = lens _curAbsStack (\s v -> s { _curAbsStack = v })

assignLens :: Assignment tp
              -> Simple Lens (MapF Assignment AbsValue) (AbsValue tp)
assignLens ass = lens (fromMaybe TopV . MapF.lookup ass)
                      (\s v -> MapF.insert ass v s)

-- | Merge in the value of the assignment.  If we have already seen a
-- value, this will combine with meet.
addAssignment :: Assignment tp -> AbsRegs -> AbsRegs
addAssignment a c =
  c & (absAssignments . assignLens a) %~ flip meet (transferRHS c (assignRhs a))

deleteRange :: Integer -> Integer -> Map Integer v -> Map Integer v
deleteRange l h m
  | h < l = m
  | otherwise =
    case Map.lookupGE l m of
      Just (k,_) | k <= h -> deleteRange (k+1) h (Map.delete k m)
      _ -> m

someValueWidth :: Value tp -> Integer
someValueWidth v =
  case valueType v of
    BVTypeRepr w -> natValue w

addMemWrite :: Value (BVType 64) -> Value tp -> AbsRegs -> AbsRegs
addMemWrite a v r =
  case (transferValue r a, transferValue r v) of
    (_,TopV) -> r
    (StackOffset s, v_abs) | [o] <- Set.toList s -> do
      let w = someValueWidth v
          e = StackEntry (valueType v) v_abs
       in r & curAbsStack %~ Map.insert o e . deleteRange o (o+w-1)
    _ -> r

addOff :: NatRepr w -> Integer -> Integer -> Integer
addOff w o v = toUnsigned w (o + v)

subOff :: NatRepr w -> Integer -> Integer -> Integer
subOff w o v = toUnsigned w (o - v)

resetRSP :: (N.RegisterName cl -> AbsValue (N.RegisterType cl))
         -> (N.RegisterName cl -> AbsValue (N.RegisterType cl))
resetRSP otherFn r
  | Just Refl <- testEquality r N.rsp = concreteStackOffset 0
  | otherwise = otherFn r

-- | Return state for after value has run.
finalAbsBlockState :: AbsRegs -> X86State Value -> AbsBlockState
finalAbsBlockState c s = do
  let mkState :: (forall cl . N.RegisterName cl -> AbsValue (N.RegisterType cl))
              -> AbsBlockStack
              -> AbsBlockState
      mkState trans newStack =
        AbsBlockState { _absX86State = mkX86State (resetRSP trans)
                      , _startAbsStack = newStack
                      }
  case transferValue c (s^.register N.rsp) of
    StackOffset offsets | [0] <- Set.toList offsets ->
      let transferReg :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
          transferReg r = transferValue c (s^.register r)
       in mkState transferReg (c^.curAbsStack)
    StackOffset offsets | [o] <- Set.toList offsets ->
      let transferReg :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
          transferReg r =
            case transferValue c (s^.register r) of
              StackOffset t -> StackOffset (Set.map (\v -> subOff n64 v o) t)
              v -> v
          newStack = Map.fromList $
            [ (subOff n64 a o, v) | (a,v) <- Map.toList (c^.curAbsStack) ]
       in mkState transferReg newStack
    SomeStackOffset ->
      let transferReg :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
          transferReg r =
            case transferValue c (s^.register r) of
              StackOffset _ -> SomeStackOffset
              v -> v
       in mkState transferReg Map.empty
    _ ->
      let transferReg :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
          transferReg r =
            case transferValue c (s^.register r) of
              StackOffset _ -> TopV
              SomeStackOffset -> TopV
              v -> v
       in mkState transferReg Map.empty

------------------------------------------------------------------------
-- Transfer functions

transferValue :: AbsRegs
              -> Value tp
              -> AbsValue tp
transferValue c v =
  case v of
   BVValue w i
     | 0 <= i && i <= maxUnsigned w -> abstractSingleton w i
     | otherwise -> error $ "transferValue given illegal value " ++ show (pretty v)
   -- Invariant: v is in m
   AssignedValue a ->
     fromMaybe (error $ "Missing assignment for " ++ show (assignId a))
               (MapF.lookup a (c^.absAssignments))
   Initial r
     | Just Refl <- testEquality r N.rsp -> do
       StackOffset (Set.singleton 0)
     | otherwise -> c ^. absInitialRegs ^. register r

transferApp :: AbsRegs
            -> App Value tp
            -> AbsValue tp
transferApp r a =
  case a of
    Trunc v w -> trunc (transferValue r v) w
    UExt  v w -> uext  (transferValue r v) w
    BVAdd w x y -> bvadd w (transferValue r x) (transferValue r y)
    BVSub w x y -> bvsub w (transferValue r x) (transferValue r y)
    BVMul w x y -> bvmul w (transferValue r x) (transferValue r y)
    _ -> top

transferRHS :: forall tp
            .  AbsRegs
            -> AssignRhs tp
            -> AbsValue tp
transferRHS r rhs =
  case rhs of
    EvalApp app    -> transferApp r app
    SetUndefined _ -> top
    Read (MemLoc a tp)
      | StackOffset s <- transferValue r a
      , [o] <- Set.toList s
      , Just (StackEntry v_tp v) <- Map.lookup o (r^.curAbsStack)
      , Just Refl <- testEquality tp v_tp ->
         v
    Read _ -> top
