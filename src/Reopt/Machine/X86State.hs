{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Reopt.Machine.X86State
  ( X86State
  , mkX86State
  , mkX86StateM
  , curIP
  , x87TopReg
  , initX86State
    -- * Combinators
  , foldX86StateValue
  , zipWithX86State
  , mapX86State
  , mapX86StateM
  , mapX86StateWithM
  , foldValueCached
    -- * X86Reg
  , X86Reg(..)
  , x86Reg
  , rax_reg
  , rbx_reg
  , rcx_reg
  , rdx_reg
  , r11_reg
  , cf_reg
  , df_reg
  , boundValue
    -- ** Pretty printing
  , PrettyRegValue(..)
    -- * ExploreLoc
  , ExploreLoc(..)
  , rootLoc
    -- * Architecture
  , X86_64
  , X86PrimFn(..)
  , X86PrimLoc(..)
  , X86Stmt(..)
    -- * X86-64 Specific functions
  , refsInAssignRhs
  , refsInValue
  , hasCallComment
  , hasRetComment
  , asStackAddrOffset
  , x86StackDelta
    -- * Lists of X86 Registers
  , gpRegList
  , x87FPURegList

  , x86StateRegs
  , x86CalleeSavedRegs
  , x86ArgumentRegs
  , x86FloatArgumentRegs
  , x86ResultRegs
  , x86FloatResultRegs
  , x86SyscallArgumentRegs
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import           GHC.TypeLits
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Macaw.CFG
import qualified Reopt.Machine.StateNames as N
import           Data.Macaw.Types

------------------------------------------------------------------------
-- X86_64 specific declarations

data X86_64
type instance ArchReg  X86_64 = X86Reg
type instance RegAddrWidth X86Reg = 64

type instance ArchFn   X86_64 = X86PrimFn
type instance ArchStmt X86_64 = X86Stmt
type instance ArchAddr X86_64 = Word64
type instance ArchCFLocation X86_64 = ExploreLoc

------------------------------------------------------------------------
-- X86PrimLoc

-- | This describes a primitive location that can be read or written to in the
-- X86 architecture model.
data X86PrimLoc tp
   = (tp ~ BVType 64) => ControlLoc !(N.RegisterName 'N.Control)
   | (tp ~ BVType 64) => DebugLoc   !(N.RegisterName 'N.Debug)
   | (tp ~ BVType 16) => FS
     -- ^ This refers to the selector of the 'FS' register.
   | (tp ~ BVType 16) => GS
     -- ^ This refers to the selector of the 'GS' register.
   | (tp ~ BVType 2)  => X87_PC
     -- ^ X87 precision control field.
     --
     -- Values are:
     -- 00 Single Precision (24 bits)
     -- 01 Reserved
     -- 10 Double Precision (53 bits)
     -- 11 Double Extended Precision (64 bits)
   | (tp ~ BVType 2) => X87_RC
     -- ^ X87 rounding control field.  Values are:
     --
     -- 00 Round to nearest (even)
     -- Rounded result is the closest to the infinitely precise result. If two
     -- values are equally close, the result is the even value (that is, the one
     -- with the least-significant bit of zero). Default
     --
     -- 01 Round down (toward −∞)
     -- Rounded result is closest to but no greater than the infinitely precise result.
     --
     -- 10 Round up (toward +∞)
     -- Rounded result is closest to but no less than the infinitely precise result.
     --
     -- 11 Round toward zero (Truncate)
     -- Rounded result is closest to but no greater in absolute value than the
     -- infinitely precise result.

instance HasRepr X86PrimLoc TypeRepr where
  typeRepr ControlLoc{} = knownType
  typeRepr DebugLoc{}   = knownType
  typeRepr FS = knownType
  typeRepr GS = knownType
  typeRepr X87_PC = knownType
  typeRepr X87_RC = knownType

instance Pretty (X86PrimLoc tp) where
  pretty (ControlLoc r) = text (show r)
  pretty (DebugLoc r) = text (show r)
  pretty FS = text "fs"
  pretty GS = text "gs"
  pretty X87_PC = text "x87_pc"
  pretty X87_RC = text "x87_rc"

------------------------------------------------------------------------
-- X86PrimFn

-- | Defines primitive functions in the X86 format.
data X86PrimFn tp
   = ReadLoc !(X86PrimLoc tp)
     -- ^ Read from a primitive X86 location
   | (tp ~ BVType 64) => ReadFSBase
     -- ^ Read the 'FS' base address
   | (tp ~ BVType 64) => ReadGSBase
     -- ^ Read the 'GS' base address
   | (tp ~ BVType 64)
     => MemCmp !Integer
               -- ^ Number of bytes per value.
               !(BVValue X86_64 64)
               -- ^ Number of values to compare
               !(BVValue X86_64 64)
               -- ^ Pointer to first buffer.
               !(BVValue X86_64 64)
               -- ^ Pointer to second buffer.
               !(BVValue X86_64 1)
               -- ^ Direction flag, False means increasing
     -- ^ Compares to memory regions
   | forall n
     . (tp ~ BVType 64, 1 <= n)
     => FindElement !Integer
                    -- ^ Number of bytes to compare at a time {1, 2, 4, 8}
                    !Bool
                    -- ^ Find first matching (True) or not matching (False)
                    !(BVValue X86_64 64)
                    -- ^ Number of elements to compare
                    !(BVValue X86_64 64)
                    -- ^ Pointer to first buffer
                    !(BVValue X86_64 n)
                    -- ^ Value to compare
                    !(BVValue X86_64 1)
                    -- ^ Flag indicates direction of copy:
                    -- True means we should decrement buffer pointers after each copy.
                    -- False means we should increment the buffer pointers after each copy.

instance HasRepr X86PrimFn TypeRepr where
  typeRepr f =
    case f of
      ReadLoc loc   -> typeRepr loc
      ReadFSBase    -> knownType
      ReadGSBase    -> knownType
      MemCmp{}      -> knownType
      FindElement{} -> knownType

instance PrettyF X86PrimFn where
  prettyF = runIdentity . ppX86PrimFn (Identity . ppValue 10)

instance PrettyArch X86_64 where
  ppArchFn = ppX86PrimFn

-- | An operation for pretty printing a 'X86PrimFn' using a pretty printer
-- for values that is implemented as a 'Applicative' action to allow side
-- effects.
ppX86PrimFn :: Applicative m
            => (forall u . Value X86_64 u -> m Doc)
               -- ^ Function for pretty printing vlaue.
            -> X86PrimFn tp
            -> m Doc
ppX86PrimFn pp f =
  case f of
    ReadLoc loc -> pure $ pretty loc
    ReadFSBase  -> pure $ text "fs.base"
    ReadGSBase  -> pure $ text "gs.base"
    MemCmp sz cnt src dest rev -> sexprA "memcmp" args
      where args = [pure (pretty sz), pp cnt, pp dest, pp src, pp rev]
    FindElement sz fndeq cnt buf val rev -> sexprA "find_element" args
      where args = [pure (pretty sz), pure (pretty fndeq), pp cnt, pp buf, pp val, pp rev]

------------------------------------------------------------------------
-- X86Stmt

-- | An X86 specific statement
data X86Stmt
   = forall tp .
     WriteLoc !(X86PrimLoc tp) !(Value X86_64 tp)
   | MemCopy !Integer
             -- ^ Number of bytes to copy at a time (1,2,4,8)
             !(BVValue X86_64 64)
             -- ^ Number of values to move.
             !(BVValue X86_64 64)
             -- ^ Start of source buffer.
             !(BVValue X86_64 64)
             -- ^ Start of destination buffer.
             !(BVValue X86_64 1)
             -- ^ Flag indicates whether direction of move:
             -- True means we should decrement buffer pointers after each copy.
             -- False means we should increment the buffer pointers after each copy.
   | forall n .
     MemSet !(BVValue X86_64 64)
            -- ^ Number of values to assign
            !(BVValue X86_64 n)
            -- ^ Value to assign
            !(BVValue X86_64 64)
            -- ^ Address to start assigning from.
            !(BVValue X86_64 1)
            -- ^ Direction flag

instance Pretty X86Stmt where
  pretty (WriteLoc loc rhs) = pretty loc <+> text ":=" <+> ppValue 0 rhs
  pretty (MemCopy sz cnt src dest rev) =
      text "memcopy" <+> parens (hcat $ punctuate comma args)
    where args = [pretty sz, pretty cnt, pretty src, pretty dest, pretty rev]
  pretty (MemSet cnt val dest df) =
      text "memset" <+> parens (hcat $ punctuate comma args)
    where args = [pretty cnt, pretty val, pretty dest, pretty df]

------------------------------------------------------------------------
-- Architecture-specific Value operations

asStackAddrOffset :: Value X86_64 tp -> Maybe (BVValue X86_64 64)
asStackAddrOffset addr
  | Just (BVAdd _ (Initial base) offset) <- valueAsApp addr
  , Just Refl <- testEquality base sp_reg = do
    Just offset
  | Initial base <- addr
  , Just Refl <- testEquality base sp_reg = do
    Just (BVValue knownNat 0)
  | otherwise =
    Nothing

------------------------------------------------------------------------
-- X86Reg

-- | Defines the set of registers that are modeled as storing values in the
-- semantics for X86_64.
data X86Reg tp
   = (BVType 64 ~ tp) => X86_IP
     -- ^ The current IP register
   | (BVType  64 ~ tp) => X86_GP !Int
     -- ^ One of 16 general purpose registers
   | (BVType   1 ~ tp) => X86_FlagReg !Int
     -- ^ One of 32 flag registers
   | (BVType   1 ~ tp) => X87_ControlReg !Int
     -- ^ One of 16 x87 control registers
   | (BVType   1 ~ tp) => X87_StatusReg !Int
     -- ^ One of 16 X87 Status registers
   | (BVType   3 ~ tp) => X87_TopReg
     -- ^ The floating point top register
   | (BVType   2 ~ tp) => X87_TagReg !Int
      -- ^ One of 8 fpu tag registers.
   | (BVType  80 ~ tp) => X87_FPUReg !Int
      -- ^ One of 8 fpu/mmx registers.
   | (BVType 128 ~ tp) => X86_XMMReg !Int
     -- ^ One of 8 XMM registers

instance Eq (X86Reg tp) where
  x == y =
    case compareF x y of
      EQF -> True
      _ -> False

instance Ord (X86Reg tp) where
  compare x y =
    case compareF x y of
      EQF -> EQ
      LTF -> LT
      GTF -> GT

instance TestEquality X86Reg where
  testEquality x y =
    case compareF x y of
      EQF -> Just Refl
      _ -> Nothing

instance OrdF X86Reg where
  compareF X86_IP X86_IP = EQF
  compareF X86_IP _      = LTF
  compareF _      X86_IP = GTF

  compareF (X86_GP i) (X86_GP j) = fromOrdering (compare i j)
  compareF X86_GP{}   _          = LTF
  compareF _          X86_GP{}   = GTF

  compareF (X86_FlagReg i) (X86_FlagReg j) = fromOrdering (compare i j)
  compareF X86_FlagReg{}   _               = LTF
  compareF _               X86_FlagReg{}   = GTF

  compareF (X87_ControlReg i) (X87_ControlReg j) = fromOrdering (compare i j)
  compareF X87_ControlReg{}   _                  = LTF
  compareF _                  X87_ControlReg{}   = GTF

  compareF (X87_StatusReg i) (X87_StatusReg j) = fromOrdering (compare i j)
  compareF X87_StatusReg{}   _                 = LTF
  compareF _                 X87_StatusReg{}   = GTF

  compareF X87_TopReg   X87_TopReg = EQF
  compareF X87_TopReg   _          = LTF
  compareF _            X87_TopReg = GTF

  compareF (X87_TagReg i) (X87_TagReg j)   = fromOrdering (compare i j)
  compareF X87_TagReg{}   _                = LTF
  compareF _                  X87_TagReg{} = GTF

  compareF (X87_FPUReg i) (X87_FPUReg j) = fromOrdering (compare i j)
  compareF X87_FPUReg{}   _              = LTF
  compareF _              X87_FPUReg{}   = GTF

  compareF (X86_XMMReg i) (X86_XMMReg j) = fromOrdering (compare i j)

-- | Map a register name to a X86Reg if it is treated as an
-- abstract register in the simulator.
x86Reg :: N.RegisterName cl -> Maybe (X86Reg (N.RegisterType cl))
x86Reg nm =
  case nm of
    N.IPReg           -> Just $! X86_IP
    N.GPReg n         -> Just $! X86_GP n
    N.FlagReg n       -> Just $! X86_FlagReg n
    N.X87ControlReg n -> Just $! X87_ControlReg n
    N.X87StatusReg n  -> Just $! X87_StatusReg n
    N.X87TopReg       -> Just $! X87_TopReg
    N.X87TagReg n     -> Just $! X87_TagReg n
    N.X87FPUReg n     -> Just $! X87_FPUReg n
    N.XMMReg n        -> Just $! X86_XMMReg n
    _ -> Nothing

rax_reg :: X86Reg (BVType 64)
rax_reg = X86_GP 0

rcx_reg :: X86Reg (BVType 64)
rcx_reg = X86_GP 1

rdx_reg :: X86Reg (BVType 64)
rdx_reg = X86_GP 2

rbx_reg :: X86Reg (BVType 64)
rbx_reg = X86_GP 3

-- | The stack pointer
x86_sp_reg :: X86Reg (BVType 64)
x86_sp_reg  = X86_GP  4

rbp_reg :: X86Reg (BVType 64)
rbp_reg = X86_GP  5

rsi_reg :: X86Reg (BVType 64)
rsi_reg = X86_GP  6

rdi_reg :: X86Reg (BVType 64)
rdi_reg = X86_GP  7

r8_reg :: X86Reg (BVType 64)
r8_reg = X86_GP  8

r9_reg :: X86Reg (BVType 64)
r9_reg = X86_GP  9

r10_reg :: X86Reg (BVType 64)
r10_reg = X86_GP 10

r11_reg :: X86Reg (BVType 64)
r11_reg = X86_GP 11

r12_reg :: X86Reg (BVType 64)
r12_reg = X86_GP 12

r13_reg :: X86Reg (BVType 64)
r13_reg = X86_GP 13

r14_reg :: X86Reg (BVType 64)
r14_reg = X86_GP 14

r15_reg :: X86Reg (BVType 64)
r15_reg = X86_GP 15

cf_reg :: X86Reg (BVType 1)
cf_reg = X86_FlagReg 0

df_reg :: X86Reg (BVType 1)
df_reg = X86_FlagReg 10

instance RegisterInfo X86Reg where
  ip_reg = X86_IP
  sp_reg = x86_sp_reg

  -- The register used to store system call numbers.
  syscall_num_reg = rax_reg

  -- The ABI defines these (http://www.x86-64.org/documentation/abi.pdf)
  -- Syscalls clobber rcx and r11, but we don't really care about these
  -- anyway.
  syscallArgumentRegs = x86SyscallArgumentRegs

-- | The ABI defines these (http://www.x86-64.org/documentation/abi.pdf)
-- Syscalls clobber rcx and r11, but we don't really care about these anyway.
x86SyscallArgumentRegs :: [ X86Reg (BVType 64) ]
x86SyscallArgumentRegs = [rdi_reg, rsi_reg, rdx_reg, r10_reg, r8_reg, r9_reg ]

instance Show (X86Reg tp) where
  show X86_IP          = "rip"
  show (X86_GP n)      = nm
    where Just nm = N.gpNames V.!? n
  show (X86_FlagReg n)    = nm
    where Just nm = N.flagNames V.!? n
  show (X87_ControlReg n)  = nm
    where Just nm = N.x87ControlNames V.!? n
  show (X87_StatusReg n) = nm
    where Just nm = N.x87StatusNames V.!? n
  show X87_TopReg      = "x87top"
  show (X87_TagReg n)  = "tag" ++ show n
  show (X87_FPUReg n)  = "fpu" ++ show n
  show (X86_XMMReg n)  = "xmm" ++ show n

instance PrettyF X86Reg where
  prettyF = text . show

instance ShowF X86Reg where
  showF = show

instance HasRepr X86Reg TypeRepr where
  typeRepr r =
    case r of
      X86_IP           -> knownType
      X86_GP{}         -> knownType
      X86_FlagReg{}    -> knownType
      X87_ControlReg{} -> knownType
      X87_StatusReg{}  -> knownType
      X87_TopReg       -> knownType
      X87_TagReg{}     -> knownType
      X87_FPUReg{}     -> knownType
      X86_XMMReg{}     -> knownType

------------------------------------------------------------------------
-- X86Reg lists

gpRegList :: [X86Reg (BVType 64)]
gpRegList = [X86_GP i | i <- [0..15]]

flagRegList :: [X86Reg (BVType 1)]
flagRegList = [X86_FlagReg i | i <- [0,2,4,6,7,8,9,10,11]]

x87ControlRegList :: [X86Reg (BVType 1)]
x87ControlRegList = [X87_ControlReg i | i <- [0..15]]

x87StatusRegList :: [X86Reg (BVType 1)]
x87StatusRegList = [X87_StatusReg i | i <- [0..15]]

x87TagRegList :: [X86Reg (BVType 2)]
x87TagRegList = [X87_TagReg i | i <- [0..7]]

x87FPURegList :: [X86Reg (BVType 80)]
x87FPURegList = [X87_FPUReg i | i <- [0..7]]

xmmRegList :: [X86Reg (BVType 128)]
xmmRegList = [X86_XMMReg i | i <- [0..7]]

-- | List of registers stored in X86State
x86StateRegs :: [Some X86Reg]
x86StateRegs
  =  [Some X86_IP]
  ++ (Some <$> gpRegList)
  ++ (Some <$> flagRegList)
  ++ (Some <$> x87ControlRegList)
  ++ (Some <$> x87StatusRegList)
  ++ [Some X87_TopReg]
  ++ (Some <$> x87TagRegList)
  ++ (Some <$> x87FPURegList)
  ++ (Some <$> xmmRegList)

------------------------------------------------------------------------
-- X86State

-- | This represents the state of the processor registers after some
-- execution.
type X86State = RegState X86_64

-- | the value of the current instruction pointer.
curIP :: Simple Lens (X86State f) (f (BVType 64))
curIP = boundValue ip_reg

-- | the value oDebugReg{}  f the current instruction pointer.
x87TopReg :: Simple Lens (X86State f) (f (BVType 3))
x87TopReg = boundValue X87_TopReg

mkX86StateM :: Applicative m
            => (forall tp . X86Reg tp -> m (f tp))
            -> m (X86State f)
mkX86StateM f = RegState . MapF.fromList <$> traverse g x86StateRegs
  where g (Some r) = MapF.Pair r <$> f r

mkX86State :: (forall tp . X86Reg tp -> f tp) -> X86State f
mkX86State f = runIdentity (mkX86StateM (return . f))

foldX86StateValue :: Monoid a => (forall u. f u -> a) -> X86State f -> a
foldX86StateValue f (RegState m) = foldMapF f m

zipWithX86State :: (forall u. f u -> g u -> h u)
                -> X86State f
                -> X86State g
                -> X86State h
zipWithX86State f x y = mkX86State (\r -> f (x ^. boundValue r) (y ^. boundValue r))

mapX86State :: (forall u. f u -> g u)
            -> X86State f
            -> X86State g
mapX86State f (RegState x) = RegState (fmapF f x)

mapX86StateM :: Monad m
             => (forall u. f u -> m (g u))
             -> X86State f
             -> m (X86State g)
mapX86StateM f (RegState x) = RegState <$> traverseF f x

mapX86StateWithM :: Monad m
                 => (forall tp. X86Reg tp -> f tp -> m (g tp))
                 -> X86State f
                 -> m (X86State g)
mapX86StateWithM f (RegState m) = RegState <$> MapF.traverseWithKey f m

------------------------------------------------------------------------
-- ExploreLoc

-- | This represents the control-flow information needed to build basic blocks
-- for a code location.
data ExploreLoc
   = ExploreLoc { loc_ip :: CodeAddr
                  -- ^ IP address.
                , loc_x87_top :: Int
                  -- ^ Top of x86 address.
                }
 deriving (Eq, Ord)

instance Pretty ExploreLoc where
  pretty loc = text $ showHex (loc_ip loc) ""

rootLoc :: CodeAddr -> ExploreLoc
rootLoc ip = ExploreLoc { loc_ip = ip
                        , loc_x87_top = 0
                        }

initX86State :: ExploreLoc -- ^ Location to explore from.
             -> X86State (Value X86_64)
initX86State loc = mkX86State Initial
                 & curIP     .~ mkLit knownNat (toInteger (loc_ip loc))
                 & x87TopReg .~ mkLit knownNat (toInteger (loc_x87_top loc))

------------------------------------------------------------------------
-- Compute set of assignIds in values.

refsInApp :: App (Value arch) tp -> Set AssignId
refsInApp app = foldApp refsInValue app

refsInValue :: Value arch tp -> Set AssignId
refsInValue (AssignedValue (Assignment v _)) = Set.singleton v
refsInValue _                                = Set.empty

refsInAssignRhs :: AssignRhs X86_64 tp -> Set AssignId
refsInAssignRhs rhs =
  case rhs of
    EvalApp v      -> refsInApp v
    SetUndefined _ -> Set.empty
    ReadMem v _    -> refsInValue v
    EvalArchFn f ->
      case f of
        ReadLoc _ -> Set.empty
        ReadFSBase -> Set.empty
        ReadGSBase -> Set.empty
        MemCmp _ cnt src dest dir ->
          Set.unions [ refsInValue cnt
                     , refsInValue src
                     , refsInValue dest
                     , refsInValue dir
                     ]
        FindElement _ _ cnt buf val dir ->
          Set.unions [ refsInValue cnt
                     , refsInValue buf
                     , refsInValue val
                     , refsInValue dir
                     ]

------------------------------------------------------------------------
-- StateMonadMonoid

-- helper type to make a monad a monoid in the obvious way
newtype StateMonadMonoid s m = SMM { getStateMonadMonoid :: State s m }
                               deriving (Functor, Applicative, Monad, MonadState s)

instance Monoid m => Monoid (StateMonadMonoid s m) where
  mempty = return mempty
  mappend m m' = do mv <- m
                    mv' <- m'
                    return (mappend mv mv')

foldValueCached :: forall m tp
                .  (Monoid m)
                => (forall n.  NatRepr n -> Integer -> m)
                -> (forall utp . ArchReg X86_64 utp -> m)
                -> (forall utp . Assignment X86_64 utp -> m -> m)
                -> Value X86_64 tp
                -> State (Map (Some (Assignment X86_64)) m) m
foldValueCached litf initf assignf val = getStateMonadMonoid (go val)
  where
    go :: forall tp'
       .  Value X86_64 tp'
       -> StateMonadMonoid (Map (Some (Assignment X86_64)) m) m
    go v =
      case v of
        BVValue sz i -> return $ litf sz i
        Initial r    -> return $ initf r
        AssignedValue asgn@(Assignment _ rhs) ->
          do m_v <- use (at (Some asgn))
             case m_v of
               Just v' -> return $ assignf asgn v'
               Nothing ->
                  do rhs_v <- goAssignRHS rhs
                     at (Some asgn) .= Just rhs_v
                     return (assignf asgn rhs_v)

    goAssignRHS :: forall tp'
                .  AssignRhs X86_64 tp'
                -> StateMonadMonoid (Map (Some (Assignment X86_64)) m) m
    goAssignRHS v =
      case v of
        EvalApp a -> foldApp go a
        SetUndefined _w -> mempty
        ReadMem addr _ -> go addr
        EvalArchFn f ->
          case f of
            ReadLoc _ -> mempty
            ReadFSBase -> mempty
            ReadGSBase -> mempty
            MemCmp _sz cnt src dest rev ->
              mconcat [ go cnt, go src, go dest, go rev ]
            FindElement _sz _findEq cnt buf val' rev ->
              mconcat [ go cnt, go buf, go val', go rev ]

------------------------------------------------------------------------
-- X86_64 specific block operations.

-- | Returns true if block has a call comment.
hasCallComment :: Block X86_64 -> Bool
hasCallComment b = any isCallComment (blockStmts b)
  where isCallComment (Comment s) = "call" `Text.isInfixOf` s
        isCallComment _ = False

-- | Returns true if block has a ret comment.
hasRetComment :: Block X86_64 -> Bool
hasRetComment b = any isRetComment (blockStmts b)
  where isRetComment (Comment s) = "ret" `Text.isSuffixOf` s
        isRetComment _ = False

------------------------------------------------------------------------
-- Register names

-- | List of registers that a callee must save.
x86CalleeSavedRegs :: Set (Some X86Reg)
x86CalleeSavedRegs = Set.fromList $
  [ -- Some N.rsp sjw: rsp is special
    Some rbp_reg
  , Some rbx_reg
  , Some r12_reg
  , Some r13_reg
  , Some r14_reg
  , Some r15_reg
  ]

x86ArgumentRegs :: [X86Reg (BVType 64)]
x86ArgumentRegs = [rdi_reg, rsi_reg, rdx_reg, rcx_reg, r8_reg, r9_reg]

x86FloatArgumentRegs :: [X86Reg (BVType 128)]
x86FloatArgumentRegs =  X86_XMMReg <$> [0..7]

x86ResultRegs :: [X86Reg (BVType 64)]
x86ResultRegs = [ rax_reg, rdx_reg ]

x86FloatResultRegs :: [X86Reg (BVType 128)]
x86FloatResultRegs = [ X86_XMMReg 0 ]

-- | How the stack pointer moves when a call is made.
x86StackDelta :: Integer
x86StackDelta = -8
