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
  ( initX86State
  , x87TopReg
    -- * Combinators
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
    -- * ExploreLoc
  , ExploreLoc(..)
  , rootLoc
    -- * Architecture
  , X86_64
  , X86PrimFn(..)
  , CanFoldValues(..)
  , SIMDWidth(..)
  , X86PrimLoc(..)
  , X86Stmt(..)
    -- * X86-64 Specific functions
  , refsInAssignRhs
  , refsInValue
  , refsInApp
  , hasCallComment
  , hasRetComment
  , asStackAddrOffset
  , x86StackDelta
    -- * Lists of X86 Registers
  , gpRegList
  , x87FPURegList
  , refsInX86PrimFn
  , x86StateRegs
  , x86CalleeSavedRegs
  , x86ArgumentRegs
  , x86FloatArgumentRegs
  , x86ResultRegs
  , x86FloatResultRegs
  , x86SyscallArgumentRegs
  , CodeAddr
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word
import qualified Flexdis86 as F
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Macaw.CFG
import           Data.Macaw.Memory ( MemWord, addrValue )
import           Data.Macaw.Types

import qualified Reopt.Machine.StateNames as N
import           Reopt.Semantics.Monad (SIMDWidth(..), RepValSize(..))


-- | An address of a code location.
--
-- This is currently just a Word64, but we may need to switch to a
-- structured representation once dynamic libraries are supported.
type CodeAddr = Word64

------------------------------------------------------------------------
-- X86_64 specific declarations

type instance RegAddrWidth X86Reg = 64

data X86_64
type instance ArchReg  X86_64 = X86Reg

type instance ArchFn   X86_64 = X86PrimFn
type instance ArchStmt X86_64 = X86Stmt

------------------------------------------------------------------------
-- X86PrimLoc

-- | This describes a primitive location that can be read or written to in the
--  X86 architecture model.
-- Primitive locations are not modeled as registers, but rather as implicit state.
data X86PrimLoc tp
   = (tp ~ BVType 64) => ControlLoc !F.ControlReg
   | (tp ~ BVType 64) => DebugLoc   !F.DebugReg
   | (tp ~ BVType 16) => FS
     -- ^ This refers to the selector of the 'FS' register.
   | (tp ~ BVType 16) => GS
     -- ^ This refers to the se lector of the 'GS' register.
   | forall w . (tp ~ BVType   w) => X87_ControlLoc !(N.X87_ControlReg w)
     -- ^ One of the x87 control registers

instance HasRepr X86PrimLoc TypeRepr where
  typeRepr ControlLoc{} = knownType
  typeRepr DebugLoc{}   = knownType
  typeRepr FS = knownType
  typeRepr GS = knownType
  typeRepr (X87_ControlLoc r) = BVTypeRepr (typeRepr r)

instance Pretty (X86PrimLoc tp) where
  pretty (ControlLoc r) = text (show r)
  pretty (DebugLoc r) = text (show r)
  pretty FS = text "fs"
  pretty GS = text "gs"
  pretty (X87_ControlLoc r) = text (show r)

------------------------------------------------------------------------
-- X86PrimFn

-- | Return the 'NatRepr' associated with the given width.
simdWidthNatRepr :: SIMDWidth w -> NatRepr w
simdWidthNatRepr SIMD_64  = knownNat
simdWidthNatRepr SIMD_128 = knownNat
simdWidthNatRepr SIMD_256 = knownNat

-- | Defines primitive functions in the X86 format.
data X86PrimFn ids tp
   = ReadLoc !(X86PrimLoc tp)
     -- ^ Read from a primitive X86 location
   | (tp ~ BVType 64) => ReadFSBase
     -- ^ Read the 'FS' base address
   | (tp ~ BVType 64) => ReadGSBase
     -- ^ Read the 'GS' base address
   | (tp ~ BVType 128) => CPUID (BVValue X86_64 ids 32)
     -- ^ The CPUID instruction
     --
     -- Given value in eax register, this returns the concatenation of eax:ebx:ecx:edx.
   | (tp ~ BVType 64) => RDTSC
     -- ^ The RDTSC instruction
     --
     -- This returns the current time stamp counter a 64-bit value that will
     -- be stored in edx:eax
   | (tp ~ BVType 64) => XGetBV (BVValue X86_64 ids 32)
     -- ^ The XGetBV instruction primitive
     --
     -- This returns the extended control register defined in the given value
     -- as a 64-bit value that will be stored in edx:eax
   | forall w
     .  (tp ~ BVType w)
     => PShufb !(SIMDWidth w) !(BVValue X86_64 ids w) !(BVValue X86_64 ids w)
     -- ^ @PShufb w x s@ returns a value @res@ generated from the bytes of @x@
     -- based on indices defined in the corresponding bytes of @s@.
     --
     -- Let @n@ be the number of bytes in the width @w@, and let @l = log2(n)@.
     -- Given a byte index @i@, the value of byte @res[i]@, is defined by
     --   @res[i] = 0 if msb(s[i]) == 1@
     --   @res[i] = x[j] where j = s[i](0..l)
     -- where @msb(y)@ returns the most-significant bit in byte @y@.

   | (tp ~ BVType 64)
     => MemCmp !Integer
               -- /\ Number of bytes per value.
               !(BVValue X86_64 ids 64)
               -- /\ Number of values to compare
               !(BVValue X86_64 ids 64)
               -- /\ Pointer to first buffer.
               !(BVValue X86_64 ids 64)
               -- /\ Pointer to second buffer.
               !(BVValue X86_64 ids 1)
               -- /\ Direction flag, False means increasing
     -- ^ Compares to memory regions
   | forall n
     . (tp ~ BVType 64)
     => RepnzScas !(RepValSize n)
                  !(BVValue X86_64 ids n)
                  !(BVValue X86_64 ids 64)
                  !(BVValue X86_64 ids 64)
     -- ^ `RepnzScas sz val base cnt` searchs through a buffer starting at
     -- `base` to find  an element `i` such that base[i] = val.
     -- Each step it increments `i` by 1 and decrements `cnt` by `1`.  It returns
     -- the final value of `cnt`.

instance HasRepr (X86PrimFn ids) TypeRepr where
  typeRepr f =
    case f of
      ReadLoc loc   -> typeRepr loc
      ReadFSBase    -> knownType
      ReadGSBase    -> knownType
      CPUID{}       -> knownType
      RDTSC{}       -> knownType
      XGetBV{}      -> knownType
      PShufb w _ _  -> BVTypeRepr (simdWidthNatRepr w)
      MemCmp{}      -> knownType
      RepnzScas{} -> knownType

instance ArchConstraints X86_64 where
  ppArchFn = ppX86PrimFn

-- | An operation for pretty printing a 'X86PrimFn' using a pretty printer
-- for values that is implemented as a 'Applicative' action to allow side
-- effects.
ppX86PrimFn :: Applicative m
            => (forall u . Value X86_64 ids u -> m Doc)
               -- ^ Function for pretty printing vlaue.
            -> X86PrimFn ids tp
            -> m Doc
ppX86PrimFn pp f =
  case f of
    ReadLoc loc -> pure $ pretty loc
    ReadFSBase  -> pure $ text "fs.base"
    ReadGSBase  -> pure $ text "gs.base"
    CPUID code  -> sexprA "cpuid" [ pp code ]
    RDTSC       -> pure $ text "rdtsc"
    XGetBV code -> sexprA "xgetbv" [ pp code ]
    PShufb _ x s -> sexprA "pshufb" [ pp x, pp s ]
    MemCmp sz cnt src dest rev -> sexprA "memcmp" args
      where args = [pure (pretty sz), pp cnt, pp dest, pp src, pp rev]
    RepnzScas _ val buf cnt  -> sexprA "first_byte_offset" args
      where args = [pp val, pp buf, pp cnt]

------------------------------------------------------------------------
-- X86Stmt

-- | An X86 specific statement.
data X86Stmt ids
   = forall tp .
     WriteLoc !(X86PrimLoc tp) !(Value X86_64 ids tp)
   | StoreX87Control !(BVValue X86_64 ids 64)
     -- ^ Store the X87 control register in the given location.
   | MemCopy !Integer
             !(BVValue X86_64 ids 64)
             !(BVValue X86_64 ids 64)
             !(BVValue X86_64 ids 64)
             !(BVValue X86_64 ids 1)
     -- ^ Copy a region of memory from a source buffer to a destination buffer.
     --
     -- In an expression @MemCopy bc v src dest dir@
     -- * @bc@ is the number of bytes to copy at a time (1,2,4,8)
     -- * @v@ is the number of values to move.
     -- * @src@ is the start of source buffer.
     -- * @dest@ is the start of destination buffer.
     -- * @dir@ is a flag that indicates whether direction of move:
     --   * 'True' means we should decrement buffer pointers after each copy.
     --   * 'False' means we should increment the buffer pointers after each copy.
   | forall n .
     MemSet !(BVValue X86_64 ids 64)
            -- /\ Number of values to assign
            !(BVValue X86_64 ids n)
            -- /\ Value to assign
            !(BVValue X86_64 ids 64)
            -- /\ Address to start assigning from.
            !(BVValue X86_64 ids 1)
            -- /\ Direction flag

instance PrettyF X86Stmt where
  prettyF (WriteLoc loc rhs) = pretty loc <+> text ":=" <+> ppValue 0 rhs
  prettyF (StoreX87Control addr) = pretty addr <+> text ":= x87_control"
  prettyF (MemCopy sz cnt src dest rev) =
      text "memcopy" <+> parens (hcat $ punctuate comma args)
    where args = [pretty sz, pretty cnt, pretty src, pretty dest, pretty rev]
  prettyF (MemSet cnt val dest df) =
      text "memset" <+> parens (hcat $ punctuate comma args)
    where args = [pretty cnt, pretty val, pretty dest, pretty df]

------------------------------------------------------------------------
-- Architecture-specific Value operations

asStackAddrOffset :: Value X86_64 ids tp -> Maybe (BVValue X86_64 ids 64)
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
x86Reg :: N.RegisterName cl -> X86Reg cl
x86Reg nm =
  case nm of
    N.IPReg           -> X86_IP
    N.GPReg n         -> X86_GP n
    N.FlagReg n       -> X86_FlagReg n
    N.X87StatusReg n  -> X87_StatusReg n
    N.X87TopReg       -> X87_TopReg
    N.X87TagReg n     -> X87_TagReg n
    N.X87FPUReg n     -> X87_FPUReg n
    N.XMMReg n        -> X86_XMMReg n

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
  archRegs = x86StateRegs

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

x87StatusRegList :: [X86Reg (BVType 1)]
x87StatusRegList = [X87_StatusReg i | i <- [0..15]]

x87TagRegList :: [X86Reg (BVType 2)]
x87TagRegList = [X87_TagReg i | i <- [0..7]]

x87FPURegList :: [X86Reg (BVType 80)]
x87FPURegList = [X87_FPUReg i | i <- [0..7]]

xmmRegList :: [X86Reg (BVType 128)]
xmmRegList = [X86_XMMReg i | i <- [0..15]]

-- | List of registers stored in X86State
x86StateRegs :: [Some X86Reg]
x86StateRegs
  =  [Some X86_IP]
  ++ (Some <$> gpRegList)
  ++ (Some <$> flagRegList)
  ++ (Some <$> x87StatusRegList)
  ++ [Some X87_TopReg]
  ++ (Some <$> x87TagRegList)
  ++ (Some <$> x87FPURegList)
  ++ (Some <$> xmmRegList)

------------------------------------------------------------------------
-- X86State

-- | This represents the state of the processor registers after some
-- execution.
-- type X86State = RegState X86Reg

-- | the value oDebugReg{}  f the current instruction pointer.
x87TopReg :: Simple Lens (RegState X86Reg f) (f (BVType 3))
x87TopReg = boundValue X87_TopReg

------------------------------------------------------------------------
-- ExploreLoc

-- | This represents the control-flow information needed to build basic blocks
-- for a code location.
data ExploreLoc
   = ExploreLoc { loc_ip      :: !(SegmentedAddr 64)
                  -- ^ IP address.
                , loc_x87_top :: !Int
                  -- ^ Top register of x87 stack
                , loc_df_flag :: !Bool
                  -- ^ Value of DF flag
                }
 deriving (Eq, Ord)

instance Pretty ExploreLoc where
  pretty loc = text $ show (loc_ip loc)

rootLoc :: SegmentedAddr 64 -> ExploreLoc
rootLoc ip = ExploreLoc { loc_ip      = ip
                        , loc_x87_top = 7
                        , loc_df_flag = False
                        }

initX86State :: ExploreLoc -- ^ Location to explore from.
             -> RegState X86Reg (Value X86_64 ids)
initX86State loc = mkRegState Initial
                 & curIP     .~ RelocatableValue knownNat (addrValue (loc_ip loc))
                 & x87TopReg .~ mkLit knownNat (toInteger (loc_x87_top loc))
                 & boundValue df_reg .~ mkLit knownNat (if (loc_df_flag loc) then 1 else 0)

------------------------------------------------------------------------
-- Compute set of assignIds in values.

refsInX86PrimFn :: X86PrimFn ids tp -> Set (Some (AssignId ids))
refsInX86PrimFn f =
  case f of
    ReadLoc _  -> Set.empty
    ReadFSBase -> Set.empty
    ReadGSBase -> Set.empty
    CPUID v    -> refsInValue v
    RDTSC      -> Set.empty
    XGetBV v   -> refsInValue v
    PShufb _ x y -> Set.union (refsInValue x) (refsInValue y)
    MemCmp _ cnt src dest dir ->
      Set.unions [ refsInValue cnt
                 , refsInValue src
                 , refsInValue dest
                 , refsInValue dir
                 ]
    RepnzScas _ val buf cnt ->
      Set.unions [ refsInValue val
                 , refsInValue buf
                 , refsInValue cnt
                 ]

refsInX86Stmt :: X86Stmt ids -> Set (Some (AssignId ids))
refsInX86Stmt (WriteLoc _ rhs) = refsInValue rhs
refsInX86Stmt (StoreX87Control addr) = refsInValue addr
refsInX86Stmt (MemCopy _ cnt src dest df) =
  Set.unions [ refsInValue cnt
             , refsInValue src
             , refsInValue dest
             , refsInValue df
             ]
refsInX86Stmt (MemSet cnt val dest df) =
  Set.unions [ refsInValue cnt
             , refsInValue val
             , refsInValue dest
             , refsInValue df
             ]

instance StmtHasRefs X86Stmt where
  refsInStmt = refsInX86Stmt

instance FnHasRefs X86PrimFn where
  refsInFn = refsInX86PrimFn

------------------------------------------------------------------------
-- StateMonadMonoid

-- helper type to make a monad a monoid in the obvious way
newtype StateMonadMonoid s m = SMM { getStateMonadMonoid :: State s m }
   deriving (Functor, Applicative, Monad, MonadState s)


instance Monoid m => Monoid (StateMonadMonoid s m) where
  mempty = return mempty
  mappend m m' = mappend <$> m <*> m'

-- | Typeclass for folding over architecture-specific values.
class CanFoldValues arch where
  -- | Folding over ArchFn values
  foldFnValues :: Monoid r
               => (forall vtp . Value arch ids vtp -> r)
               -> ArchFn arch ids tp
               -> r

foldAssignRHSValues :: (Monoid r, CanFoldValues arch)
                    => (forall vtp . Value arch ids vtp -> r)
                    -> AssignRhs arch ids tp
                    -> r
foldAssignRHSValues go v =
  case v of
    EvalApp a -> foldApp go a
    SetUndefined _w -> mempty
    ReadMem addr _ -> go addr
    EvalArchFn f _ -> foldFnValues go f

-- | This folds over elements of a values in a  values.
--
-- It memoizes values so that it only evaluates assignments with the same id
-- once.
foldValueCached :: forall m arch ids tp
                .  (Monoid m, CanFoldValues arch)
                => (forall n.  NatRepr n -> Integer -> m)
                   -- ^ Function for literals
                -> (forall n.  NatRepr n -> MemWord (RegAddrWidth (ArchReg arch)) -> m)
                   -- ^ Function for memwords
                -> (forall utp . ArchReg arch utp -> m)
                   -- ^ Function for input registers
                -> (forall utp . AssignId ids utp -> m -> m)
                   -- ^ Function for assignments
                -> Value arch ids tp
                -> State (Map (Some (AssignId ids)) m) m
foldValueCached litf rwf initf assignf = getStateMonadMonoid . go
  where
    go :: forall tp'
       .  Value arch ids tp'
       -> StateMonadMonoid (Map (Some (AssignId ids)) m) m
    go v =
      case v of
        BVValue sz i -> return $ litf sz i
        RelocatableValue w a -> pure $ rwf w a
        Initial r    -> return $ initf r
        AssignedValue (Assignment a_id rhs) -> do
          m_v <- use (at (Some a_id))
          case m_v of
            Just v' ->
              return $ assignf a_id v'
            Nothing -> do
              rhs_v <- foldAssignRHSValues go rhs
              at (Some a_id) .= Just rhs_v
              return (assignf a_id rhs_v)

instance CanFoldValues X86_64 where
  foldFnValues go f =
    case f of
      ReadLoc _  -> mempty
      ReadFSBase -> mempty
      ReadGSBase -> mempty
      CPUID v    -> go v
      RDTSC      -> mempty
      XGetBV v   -> go v
      PShufb _ x y -> mconcat [ go x, go y ]
      MemCmp _sz cnt src dest rev ->
        mconcat [ go cnt, go src, go dest, go rev ]
      RepnzScas _sz val buf cnt ->
        mconcat [ go val, go buf, go cnt ]

------------------------------------------------------------------------
-- X86_64 specific block operations.

-- | Returns true if block has a call comment.
hasCallComment :: Block X86_64 ids -> Bool
hasCallComment b = any isCallComment (blockStmts b)
  where isCallComment (Comment s) = "call" `Text.isInfixOf` s
        isCallComment _ = False

-- | Returns true if block has a ret comment.
hasRetComment :: Block X86_64 ids -> Bool
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
  , Some X87_TopReg
  , Some df_reg
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
