------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.Monad
-- Description      : This defines the typeclasses that must be implemented
--                    to obtain semantics from x86 instructions.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- Defines typeclasses used to implement x86 instruction semantics.
------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.Semantics.Monad
  ( -- * Type
    Type(..)
  , type BVType
  , BoolType
  , FloatType
  , XMMType
  , TypeRepr(..)
  , type_width
  , KnownType(..)
  , FloatInfo(..)
  , FloatInfoRepr(..)
  , FloatInfoBits
  , floatInfoBits
    -- * RegisterView
  , RegisterView
  , RegisterViewType(..)
  , registerViewRead
  , registerViewWrite
  , registerViewBase
  , registerViewSize
  , registerViewReg
  , registerViewType
  , registerViewAsFullRegister
    -- * Location
  , Location(..)
  , loc_type
  , loc_width
  , ppLocation
  , x87reg_mmx
  , fullRegister
  , reg_low8
  , reg_high8
  , reg_low16
  , reg_low32

  , cf_loc
  , pf_loc
  , af_loc
  , zf_loc
  , sf_loc
  , tf_loc
  , if_loc
  , df_loc
  , of_loc
    -- X87
  , c0_loc
  , c1_loc
  , c2_loc
  , c3_loc

  , mkBVAddr
  , mkFPAddr
  , packWord
  , unpackWord
  -- ** Registers
  , rsp, rbp, rax, rdx, rsi, rdi, rcx, rip
  , cx,  ecx
    -- * IsLeq utility
  , IsLeq
  , n1, n8, n16, n32, n64, n80, n128
    -- * Value operations
  , IsValue(..)
  , bvKLit
  , (.+)
  , (.-)
  , (.*)
  , Pred
    -- * Semantics
  , Semantics(..)
  , Value
  , MLocation
  , SupportedBVWidth
  , IsLocationBV
  , FullSemantics
  , ExceptionClass(..)
  , Primitive(..)
    -- * Re-exports
  , type (TypeLits.<=)
  , type Flexdis86.Sizes.SizeConstraint(..)
  , module Flexdis86.Segment
  ) where

import           Data.Bits (shiftL)
import           Data.Char (toLower)
import           GHC.TypeLits as TypeLits
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import           Reopt.Machine.StateNames (RegisterName, RegisterClass(..))
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import qualified Reopt.Utils.GHCBug as GHCBug

import           Flexdis86.Sizes (SizeConstraint(..))
import           Flexdis86.Segment
  ( Segment
  , pattern ES
  , pattern CS
  , pattern SS
  , pattern DS
  , pattern FS
  , pattern GS
  )

------------------------------------------------------------------------
-- Sub registers
--
-- See comments above 'registerViewRead'.

-- | A view into a register / a subregister.
data RegisterView cl b n =
  (b + n <= N.RegisterClassBits cl) =>
  RegisterView
    { _registerViewBase :: NatRepr b
    , _registerViewSize :: NatRepr n
    , _registerViewReg :: RegisterName cl
    , _registerViewType :: RegisterViewType cl b n
    }

-- TODO(conathan) use constraint kinds and constraint aliases to name
-- the read and write constraints, instead of repeating them over and
-- over. Or better, eliminate the pointless (?) @1 <= n@ contraints
-- from 'IsValue' ...

-- | The different kinds of register views.
--
-- We introduce this "tag" type, vs embedding the read and write
-- functions directly in the 'RegisterView', in order to reify the
-- read and write functions: these tags can be inspected at run time,
-- but the corresponding read and write functions cannot. The
-- 'registerViewRead' and 'registerViewWrite' functions below map a
-- 'RegisterView' to it's corresponding read and write functions,
-- using the constraints embedded in the 'RegisterViewType' tag.
--
-- We could optimize the common case of full registers by introducing
-- a @IdentityView@ of type @RegisterViewType cl 0
-- (N.registerclassbits cl)@ whose read view is @\x -> x@ and write
-- view is @\x y -> y@.
data RegisterViewType cl (b :: Nat) (n :: Nat) =
  ( 1 <= n
  , 1 <= N.RegisterClassBits cl
  , n <= N.RegisterClassBits cl
  ) =>
  DefaultView |
  ( 1 <= n
  , 1 <= N.RegisterClassBits cl
  , n <= N.RegisterClassBits cl
  , 1 <= N.RegisterClassBits cl - n
  , N.RegisterClassBits cl - n <= N.RegisterClassBits cl
  , ((N.RegisterClassBits cl - n) + n) ~ N.RegisterClassBits cl
  , b ~ 0
  ) =>
  OneExtendOnWrite |
  ( 1 <= n
  , 1 <= N.RegisterClassBits cl
  , n <= N.RegisterClassBits cl
  , 1 <= N.RegisterClassBits cl - n
  , N.RegisterClassBits cl - n <= N.RegisterClassBits cl
  , ((N.RegisterClassBits cl - n) + n) ~ N.RegisterClassBits cl
  , b ~ 0
  ) =>
  ZeroExtendOnWrite

-- * Destructors for 'RegisterView's.

registerViewBase :: RegisterView cl b n -> NatRepr b
registerViewBase = _registerViewBase

registerViewSize :: RegisterView cl b n -> NatRepr n
registerViewSize = _registerViewSize

registerViewReg :: RegisterView cl b n -> RegisterName cl
registerViewReg = _registerViewReg

registerViewType :: RegisterView cl b n -> RegisterViewType cl b n
registerViewType = _registerViewType

-- | View a 'RegisterView' as a full register.
--
-- The returned equalities help with type checking, e.g. by
-- constraining the type indices of the 'Location' in which the
-- 'RegisterView' is embedded.
registerViewAsFullRegister ::
  RegisterView cl b n -> Maybe (RegisterName cl, b :~: 0, n :~: N.RegisterClassBits cl)
registerViewAsFullRegister (RegisterView {..})
  | Just Refl <- _registerViewBase `testEquality` n0
  , Just Refl <- _registerViewSize `testEquality` N.registerWidth _registerViewReg
  , DefaultView <- _registerViewType
  = Just (_registerViewReg, Refl, Refl)
  | otherwise = Nothing


-- * Read and write views for 'RegisterView's.

-- | Read a register via a view.
--
-- The read and write operations specify how to read and write a
-- subregister value based on the current value @v0@ in the full
-- register. The write operation also depends on the new subregister
-- value @v@ to be written.
--
-- See 'defaultRegisterViewRead' and 'defaultRegisterViewWrite'
-- implement the common case (e.g. for @ax@ as a subregister of
-- @rax@).
--
-- The special cases motivating introduction of the 'RegisterView'
-- data type are:
--
-- * 32-bit subregisters of 64-bit general purpose registers
--   (e.g. @eax@ as a subregister of @rax@), where the high-order 32
--   bits should be zeroed out on writes
--
-- * 64-bit mmx subregisters of x87 registers, where the high-order 16
--   bits should be oned out on writes.
--
-- Note that some instructions, such as @movss@ and @movsd@, specify
-- what appears to be special treatment of subregisters as part of
-- their semantics. We don't expect these *explicit* instruction
-- semantics to be implemented using 'RegisterView'. Rather,
-- 'RegisterView' is intended to implement the *implicit* special
-- treatment of some aliased registers, namely the two cases mentioned
-- above. (But this distinction is arbitrary, and we could simplify
-- some semantic implementations by treating the lower-half of an XMM
-- register as a named subregister, using a 'RegisterView').
--
-- Note: there is no type-level relationship between the base @b@ and
-- size @n@ params and the read/write views, but the base and size are
-- expected to specify which bits are read by read view.
--
-- MAYBE TODO: the read and write views have an @IsValue v@
-- constraint, but the implementations only rely on a small subset of
-- the 'IsValue' operations. So, it might make sense to factor these
-- operations out into a separate class. The operations we need for
-- reads and writes are subset of the the basic bitvector operations;
-- we need:
--
-- - 'bvLit': create a literal representing a number
-- - 'bvShl', 'bvShr': logical shifts
-- - 'bvTrunc': truncation
-- - 'bv_width': number of bits in the vector
-- - 'complement': complement the bits
-- - 'uext'': zero extension
-- - '(.|.)': bit-wise OR
--
-- If we factored this out into a separate class, it would probably
-- make sense to include a few more "basic bitvector operations" in
-- the new class:
--
-- - bit-wise AND
-- - arithmetic shift
-- - signed extension
--
-- Note that we don't need arithmetic on bit vectors, neither as ints
-- nor as floats.
registerViewRead ::
  IsValue v =>
  RegisterView cl b n -> v (N.RegisterType cl) -> v (BVType n)
registerViewRead (RegisterView {..}) =
  case _registerViewType of
    DefaultView -> defaultRegisterViewRead b n rn
    OneExtendOnWrite -> defaultRegisterViewRead b n rn
    ZeroExtendOnWrite -> defaultRegisterViewRead b n rn
  where
    b = _registerViewBase
    n = _registerViewSize
    rn = _registerViewReg

-- | Write a register via a view.
registerViewWrite ::
  IsValue v =>
  RegisterView cl b n ->
  v (N.RegisterType cl) ->
  v (BVType n) ->
  v (N.RegisterType cl)
registerViewWrite r@(RegisterView {..}) =
  case _registerViewType of
    DefaultView
      | Just (_, Refl, Refl) <- registerViewAsFullRegister r -> \ _v0 v -> v
      | otherwise -> defaultRegisterViewWrite b n rn
    OneExtendOnWrite ->
      let ones = complement (bvLit (cl `subNat` n) (0::Integer)) in
      constUpperBitsRegisterViewWrite n ones rn
    ZeroExtendOnWrite ->
      let zeros = bvLit (cl `subNat` n) (0::Integer) in
      constUpperBitsRegisterViewWrite n zeros rn
  where
    b = _registerViewBase
    n = _registerViewSize
    rn = _registerViewReg
    cl = N.registerWidth rn

-- | Extract 'n' bits starting at base 'b'.
--
-- Assumes a big-endian 'IsValue' semantics.
defaultRegisterViewRead ::
  ( 1 <= N.RegisterClassBits cl
  , 1 <= n
  , n <= N.RegisterClassBits cl
  , IsValue v
  ) =>
  NatRepr b ->
  NatRepr n ->
  RegisterName cl ->
  v (N.RegisterType cl) ->
  v (BVType n)
defaultRegisterViewRead b n _rn v0 =
  bvTrunc n $ v0 `bvShr` bvLit (bv_width v0) (natValue b)

-- | Update the 'n' bits starting at base 'b', leaving other bits
-- unchanged.
--
-- Assumes a big-endian 'IsValue' semantics.
defaultRegisterViewWrite :: forall b cl n v .
  ( 1 <= N.RegisterClassBits cl
  , 1 <= n
  , n <= N.RegisterClassBits cl
  , IsValue v
  ) =>
  NatRepr b ->
  NatRepr n ->
  RegisterName cl ->
  v (N.RegisterType cl) ->
  v (BVType n) ->
  v (N.RegisterType cl)
defaultRegisterViewWrite b n rn v0 v =
  -- Truncation 'bvTrunc' requires that the result vector has non-zero
  -- length, so we build the concatenation
  --
  --   h ++ m ++ l
  --
  -- using bitwise OR:
  --
  --   (h ++ 0^|m ++ l|)     ||
  --   (0^|h| ++ m ++ 0^|l|) ||
  --   (0^|h ++ m| ++ l)
  highOrderBits .|. middleOrderBits .|. lowOrderBits
  where
    -- | Mask out bits 'i' through 'i + j - 1'.
    --
    -- Here 'i' is the number of bits in the low-order third and 'j'
    -- is the number of bits in the middle-order third.
    --
    -- Assumes big-endian representation: left-shift moves bits to
    -- higher order and right-shift moves bits to lower order.
    mask :: (1 <= m) => Integer -> Integer -> v (BVType m) -> v (BVType m)
    mask i j x0 = x3
      where
        x1 = x0 `bvShr` bvLit w i
        x2 = x1 `bvShl` bvLit w (i + k)
        x3 = x2 `bvShr` bvLit w k

        w = bv_width x0
        -- | The number of bits in the high-order third.
        k = natValue w - i - j

    cl = N.registerWidth rn

    n', b', cl' :: Integer
    n' = natValue n
    b' = natValue b
    cl' = natValue cl

    highOrderBits   = mask (b' + n') (cl' - b' - n') v0
    middleOrderBits = uext cl v `bvShl` bvLit cl b'
    lowOrderBits    = mask 0 b' v0

-- | Update the lower 'n' bits and set the upper bits to a constant.
--
-- Assumes a big-endian 'IsValue' semantics.
constUpperBitsRegisterViewWrite :: forall cl n v .
  ( 1 <= n
  , 1 <= N.RegisterClassBits cl
  , 1 <= N.RegisterClassBits cl - n
  , N.RegisterClassBits cl - n <= N.RegisterClassBits cl
  , ((N.RegisterClassBits cl - n) + n) ~ N.RegisterClassBits cl
  , IsValue v
  ) =>
  NatRepr n ->
  v (BVType (N.RegisterClassBits cl - n)) -> -- ^ Constant bits.
  RegisterName cl ->
  v (N.RegisterType cl) ->
  v (BVType n) ->
  v (N.RegisterType cl)
constUpperBitsRegisterViewWrite _n c _rn _v0 v = bvCat c v

-- | The view for full registers.
--
-- The read view reads all bits and the write view writes all bits.
identityRegisterView ::
  (1 <= N.RegisterClassBits cl) =>
  RegisterName cl -> RegisterView cl 0 (N.RegisterClassBits cl)
identityRegisterView rn =
  RegisterView
    { _registerViewBase = b
    , _registerViewSize = n
    , _registerViewReg = rn
    , _registerViewType = DefaultView
    }
  where
    b = knownNat
    n = N.registerWidth rn

-- | The view for subregisters which are a slice of a full register.
sliceRegisterView ::
  ( 1 <= n
  , 1 <= N.RegisterClassBits cl
  , n <= N.RegisterClassBits cl
  , b + n <= N.RegisterClassBits cl
  ) =>
  NatRepr b -> NatRepr n -> RegisterName cl -> RegisterView cl b n
sliceRegisterView b n rn =
  RegisterView
    { _registerViewBase = b
    , _registerViewSize = n
    , _registerViewReg = rn
    , _registerViewType = DefaultView
    }

-- | The view for 32-bit general purpose and mmx registers.
--
-- These are the special / weird sub registers where the upper bits of
-- the underlying full register are implicitly set to a constant on
-- writes.
constUpperBitsOnWriteRegisterView ::
  ( 1 <= n
  , 1 <= N.RegisterClassBits cl
  , 1 <= N.RegisterClassBits cl - n
  , N.RegisterClassBits cl - n <= N.RegisterClassBits cl
  , n <= N.RegisterClassBits cl
  , ((N.RegisterClassBits cl - n) + n) ~ N.RegisterClassBits cl
  ) =>
  NatRepr n ->
  RegisterViewType cl 0 n ->
  RegisterName cl ->
  RegisterView cl 0 n
constUpperBitsOnWriteRegisterView n rt rn  =
  RegisterView
    { _registerViewBase = n0
    , _registerViewSize = n
    , _registerViewReg = rn
    , _registerViewType = rt
    }

------------------------------------------------------------------------
-- Location

-- | This returns the type associated with values that can be read
-- or assigned for the semantics monad.
data Location addr (tp :: Type) where
  -- A location in the virtual address space of the process.
  MemoryAddr :: addr -> TypeRepr tp  -> Location addr tp

  Register :: RegisterView cl b n -> Location addr (BVType n)

  -- The register stack: the argument is an offset from the stack
  -- top, so X87Register 0 is the top, X87Register 1 is the second,
  -- and so forth.
  X87StackRegister :: !Int -> Location addr (FloatType X86_80Float)

-- Equality and ordering.

compareRegisterViewType :: RegisterViewType cl b n -> RegisterViewType cl' b' n' -> Ordering
DefaultView `compareRegisterViewType` DefaultView = EQ
DefaultView `compareRegisterViewType` _ = LT
_ `compareRegisterViewType` DefaultView = GT
OneExtendOnWrite `compareRegisterViewType` OneExtendOnWrite = EQ
OneExtendOnWrite `compareRegisterViewType` _ = LT
_ `compareRegisterViewType` OneExtendOnWrite = GT
ZeroExtendOnWrite `compareRegisterViewType` ZeroExtendOnWrite = EQ

compareRegisterView :: RegisterView cl b n -> RegisterView cl' b' n' -> Ordering
compareRegisterView rv rv' =
  case ( _registerViewBase rv `compareF` _registerViewBase rv'
       , _registerViewSize rv `compareF` _registerViewSize rv'
       , _registerViewReg rv `compareF` _registerViewReg rv'
       ) of
    (LTF, _, _) -> LT
    (GTF, _, _) -> GT
    (EQF, LTF, _) -> LT
    (EQF, GTF, _) -> GT
    (EQF, EQF, LTF) -> LT
    (EQF, EQF, GTF) -> GT
    (EQF, EQF, EQF) ->
      _registerViewType rv `compareRegisterViewType` _registerViewType rv'

instance Ord addr => TestEquality (Location addr) where
  testEquality l l'
    | EQF <- l `compareF` l' = Just Refl
    | otherwise = Nothing

instance Eq addr => EqF (Location addr) where
  MemoryAddr addr tp `eqF` MemoryAddr addr' tp'
    | Just Refl <- testEquality tp tp' = addr == addr'
  Register rv `eqF` Register rv'
    | EQ <- rv `compareRegisterView` rv' = True
  X87StackRegister i `eqF` X87StackRegister i' = i == i'
  _ `eqF` _ = False

instance Eq addr => Eq (Location addr tp) where
  (==) = eqF

instance Ord (RegisterViewType cl b n) where
  compare = compareRegisterViewType

instance Ord (RegisterView cl b n) where
  compare = compareRegisterView

instance Eq (RegisterViewType cl b n) where
  rvt == rvt'
    | EQ <- rvt `compareRegisterViewType` rvt' = True
    | otherwise = False

instance Eq (RegisterView cl b n) where
  rv == rv'
    | EQ <- rv `compareRegisterView` rv' = True
    | otherwise = False

instance Ord addr => OrdF (Location addr) where
  MemoryAddr addr tp `compareF` MemoryAddr addr' tp' =
    case tp `compareF` tp' of
      LTF -> LTF
      EQF -> fromOrdering $ addr `compare` addr'
      GTF -> GTF
  MemoryAddr _ _ `compareF` _ = GTF
  _ `compareF` MemoryAddr _ _ = LTF
  Register rv `compareF` Register rv'
    | Just Refl <-
        _registerViewBase rv `testEquality` _registerViewBase rv'
    , Just Refl <-
        _registerViewSize rv `testEquality` _registerViewSize rv'
    , Just Refl <-
        _registerViewReg rv `testEquality` _registerViewReg rv'
    , EQ <- _registerViewType rv `compare` _registerViewType rv'
    = EQF
    | otherwise = case rv `compareRegisterView` rv' of
        GT -> GTF
        LT -> LTF
        -- This case is impossible since we already checked for
        -- equality above.
        EQ -> error "Reopt.Semantics.Monad.OrdF (Location addr).compareF: impossible!"
  Register _ `compareF` _ = GTF
  _ `compareF` Register _ = LTF
  X87StackRegister i `compareF` X87StackRegister i' =
    fromOrdering $ compare i i'

-- | Pretty print 'S.Location'.
--
-- Note: this pretty printer ignores the embedded view functions in
-- 'RegisterView's, so the pretty printed value only indicates which
-- bits are in the view, not how the view is actually defined
--
-- Going back to pretty names for subregisters is pretty ad hoc;
-- see table at http://stackoverflow.com/a/1753627/470844. E.g.,
-- instead of @%ah@, we produce @%rax[8:16]@.
ppLocation :: forall addr tp. (addr -> Doc) -> Location addr tp -> Doc
ppLocation ppAddr loc = case loc of
  MemoryAddr addr _tr -> ppAddr addr
  Register rv -> ppReg rv
  X87StackRegister i -> text $ "x87_stack@" ++ show i
  where
    -- | Print subrange as Python-style slice @<location>[<low>:<high>]@.
    --
    -- The low bit is inclusive and the high bit is exclusive, but I
    -- can't bring myself to generate @<reg>[<low>:<high>)@ :)
    ppReg :: RegisterView b n cl -> Doc
    ppReg rv =
      text $ "%" ++ show (_registerViewReg rv) ++
        if b == 0 && s == (fromIntegral $ natValue (N.registerWidth $ _registerViewReg rv))
        then ""
        else "[" ++ show b ++ ":" ++ show s ++ "]"
      where
        b = natValue $ _registerViewBase rv
        s = natValue $ _registerViewSize rv

------------------------------------------------------------------------
-- Register-location smart constructors.

-- | Full register location.
fullRegister ::
  (1 <= N.RegisterClassBits cl) =>
  N.RegisterName cl -> Location addr (N.RegisterType cl)
fullRegister = Register . identityRegisterView

-- | Subregister location for simple subregisters.
--
-- I.e., a subregister which reads and writes @n@ bits at offset @b@,
-- and preserves remaining bits on writes.
subRegister ::
  ( 1 <= n
  , 1 <= N.RegisterClassBits cl
  , n <= N.RegisterClassBits cl
  , b + n <= N.RegisterClassBits cl
  ) =>
  NatRepr b -> NatRepr n -> RegisterName cl -> Location addr (BVType n)
subRegister b n = Register . sliceRegisterView b n

-- | The view for 32-bit general purpose and mmx registers.
--
-- These are the special / weird sub registers where the upper bits of
-- the underlying full register are implicitly set to a constant on
-- writes.
constUpperBitsOnWriteRegister ::
  ( 1 <= n
  , 1 <= N.RegisterClassBits cl
  , 1 <= N.RegisterClassBits cl - n
  , N.RegisterClassBits cl - n <= N.RegisterClassBits cl
  , n <= N.RegisterClassBits cl
  , ((N.RegisterClassBits cl - n) + n) ~ N.RegisterClassBits cl
  ) =>
  NatRepr n ->
  RegisterViewType cl 0 n ->
  RegisterName cl ->
  Location addr (BVType n)
constUpperBitsOnWriteRegister n rt =
  Register . constUpperBitsOnWriteRegisterView n rt

------------------------------------------------------------------------
-- Operations on locations.

loc_type :: Location addr tp -> TypeRepr tp
loc_type (MemoryAddr _ tp) = tp
loc_type (Register rv)     = BVTypeRepr $ _registerViewSize rv
loc_type (X87StackRegister _) = knownType

loc_width :: Location addr (BVType n) -> NatRepr n
loc_width (loc_type -> BVTypeRepr nr) = nr

------------------------------------------------------------------------
-- Specific locations.

-- | CF flag
cf_loc :: Location addr BoolType
cf_loc = fullRegister N.cf

-- | PF flag
pf_loc :: Location addr BoolType
pf_loc = fullRegister N.pf

-- | AF flag
af_loc :: Location addr BoolType
af_loc = fullRegister N.af

-- | ZF flag
zf_loc :: Location addr BoolType
zf_loc = fullRegister N.zf

-- | SF flag
sf_loc :: Location addr BoolType
sf_loc = fullRegister N.sf

-- | TF flag
tf_loc :: Location addr BoolType
tf_loc = fullRegister N.tf

-- | IF flag
if_loc :: Location addr BoolType
if_loc = fullRegister N.iflag

-- | DF flag
df_loc :: Location addr BoolType
df_loc = fullRegister N.df

-- | OF flag
of_loc :: Location addr BoolType
of_loc = fullRegister N.oflag

-- | x87 flags
c0_loc, c1_loc, c2_loc, c3_loc :: Location addr BoolType
c0_loc = fullRegister N.x87c0
c1_loc = fullRegister N.x87c1
c2_loc = fullRegister N.x87c2
c3_loc = fullRegister N.x87c3

-- | Tuen an address into a location of size @n
mkBVAddr :: NatRepr n -> addr -> Location addr (BVType n)
mkBVAddr sz addr = MemoryAddr addr (BVTypeRepr sz)

-- | Tuen an address into a location of size @n
mkFPAddr :: FloatInfoRepr flt -> addr -> Location addr (FloatType flt)
mkFPAddr fir addr = MemoryAddr addr (BVTypeRepr (floatInfoBits fir))

-- | Return MMX register corresponding to x87 register.
--
-- An MMX register is the low 64-bits of an x87 register. These
-- registers have special semantics, defined in Volume 1 of the Intel
-- x86 manual: on write,the upper 16 bits of the underlying x87
-- register are oned out!
x87reg_mmx :: RegisterName 'X87_FPU -> Location addr (BVType 64)
x87reg_mmx r = constUpperBitsOnWriteRegister n64 OneExtendOnWrite r

-- | Return low 32-bits of register e.g. rax -> eax
--
-- These subregisters have special semantics, defined in Volume 1 of
-- the Intel x86 manual: on write, the upper 32 bits are zeroed out!
reg_low32 :: RegisterName 'GP -> Location addr (BVType 32)
reg_low32 r = constUpperBitsOnWriteRegister n32 ZeroExtendOnWrite r

-- | Return low 16-bits of register e.g. rax -> ax
reg_low16 :: RegisterName 'GP -> Location addr (BVType 16)
reg_low16 r = subRegister n0 n16 r

-- | Return low 8-bits of register e.g. rax -> al
reg_low8 :: RegisterName 'GP -> Location addr (BVType 8)
reg_low8 r = subRegister n0 n8 r

-- | Return bits 8-15 of the register e.g. rax -> ah
reg_high8 :: RegisterName 'GP -> Location addr (BVType 8)
reg_high8 r = subRegister n8 n8 r

rsp, rbp, rax, rdx, rsi, rdi, rcx :: Location addr (BVType 64)
rax = fullRegister N.rax
rsp = fullRegister N.rsp
rbp = fullRegister N.rbp
rdx = fullRegister N.rdx
rsi = fullRegister N.rsi
rdi = fullRegister N.rdi
rcx = fullRegister N.rcx

cx :: Location addr (BVType 16)
cx = reg_low16 N.rcx

ecx :: Location addr (BVType 32)
ecx = reg_low32 N.rcx

rip :: Location addr (BVType 64)
rip = fullRegister N.IPReg

------------------------------------------------------------------------

packWord :: forall m n. (Semantics m, 1 <= n) => N.BitPacking n -> m (Value m (BVType n))
packWord (N.BitPacking sz bits) =
  do injs <- mapM getMoveBits bits
     return (foldl1 (.|.) injs)
  where
    getMoveBits :: N.BitConversion n -> m (Value m (BVType n))
    getMoveBits (N.ConstantBit b off)
      = return $ bvLit sz (if b then 1 `shiftL` widthVal off else (0 :: Integer))
    getMoveBits (N.RegisterBit reg off)
      = do v <- uext sz <$> get (fullRegister reg)
           return $ v `bvShl` bvLit sz (widthVal off)

unpackWord :: forall m n. (Semantics m, 1 <= n) => N.BitPacking n -> Value m (BVType n) -> m ()
unpackWord (N.BitPacking sz bits) v = mapM_ unpackOne bits
  where
    unpackOne :: N.BitConversion n -> m ()
    unpackOne N.ConstantBit{}         = return ()
    unpackOne (N.RegisterBit reg off) = do
      let res_w = N.registerWidth reg
      fullRegister reg .= bvTrunc res_w (v `bvShr` bvLit sz (widthVal off))

------------------------------------------------------------------------
-- Values

-- | @IsValue@ is a class used to define expressions.
--
-- The @IsValue@ operations have BIG-ENDIAN semantics: the higher
-- order bits are on the left. So, for example, a left shift makes a
-- number large (ignoring truncation), a right shift makes a number
-- smaller, and the first argument to concatenation becomes the
-- high-order bits in the result.
class IsValue (v  :: Type -> *) where

  -- | Returns the width of a bit-vector value.
  bv_width :: v (BVType n) -> NatRepr n

  -- | Choose a value based upon the boolean (basically a pure if then else)
  mux :: v BoolType -> v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Construct a literal bit vector.  The result is undefined if the
  -- literal does not fit withint the given number of bits.
  bvLit :: (Integral a, 1 <= n) => NatRepr n -> a -> v (BVType n)

  -- | Add two bitvectors together dropping overflow.
  bvAdd :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Subtract two vectors, ignoring underflow.
  bvSub :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Performs a multiplication of two bitvector values.
  bvMul :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | 2's complement
  bvNeg :: (1 <= n) => v (BVType n) -> v (BVType n)
  bvNeg n = bvLit (bv_width n) (0 :: Int) `bvSub` n

  -- | Bitwise complement
  complement :: (1 <= n) => v (BVType n) -> v (BVType n)

  -- | Bitwise and
  (.&.) :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Bitwise or
  (.|.) :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Exclusive or
  bvXor :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Equality
  (.=.) :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  bv .=. bv' = is_zero (bv `bvXor` bv')

  -- | Inequality
  (.=/=.) :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  bv .=/=. bv' = complement (bv .=. bv')

  -- | Return true if value is zero.
  is_zero :: (1 <= n) => v (BVType n) -> v BoolType
  is_zero x = x .=. bvLit (bv_width x) (0::Integer)

  -- | Concatentates two bit vectors.
  --
  -- Big-endian, so higher-order bits come from first argument.
  bvCat :: forall m n . (1 <= m, 1 <= n) => v (BVType m) -> v (BVType n) -> v (BVType (m + n))
  bvCat h l =
    case _1_le_m_plus_n of
      LeqProof -> go
    where
      -- GHC 7.10 has a context stack overflow related to @1 <= m + n@
      -- which goes away when we factor the body out like this.
      go :: (1 <= m + n) => v (BVType (m + n))
      go =
        case ( m_le_m_plus_n , n_le_m_plus_n , _1_le_m_plus_n ) of
          (LeqProof, LeqProof, LeqProof) ->
            let highOrderBits =
                  uext m_plus_n h `bvShl` bvLit m_plus_n (widthVal $ n)
                lowOrderBits = uext m_plus_n l
            in highOrderBits .|. lowOrderBits

      m :: NatRepr m
      m = bv_width h

      n :: NatRepr n
      n = bv_width l

      m_plus_n :: NatRepr (m + n)
      m_plus_n = addNat m n

      m_le_m_plus_n :: LeqProof m (m + n)
      m_le_m_plus_n = addIsLeq m n

      n_le_m_plus_n :: LeqProof n (m + n)
      n_le_m_plus_n = addPrefixIsLeq m n

      _1_le_m_plus_n :: LeqProof 1 (m + n)
      _1_le_m_plus_n =
        leqAdd (LeqProof :: LeqProof 1 m) n

  -- | Splits a bit vectors into two.
  --
  -- Big-endian, so higher-order bits make up first component of
  -- result.
  bvSplit :: (1 <= n) => v (BVType (n + n)) -> (v (BVType n), v (BVType n))
  -- bvSplit v = (bvTrunc:: sz (bvShr (widthVal sz) v), bvTrunc sz v)
  --   where
  --     sz = halfNat (bv_width v)

  -- | Vectorization
  bvVectorize :: forall k n
               . (1 <= k, 1 <= n)
              => NatRepr k
              -> v (BVType n)
              -> [v (BVType k)]
  bvVectorize sz bv
    | Just Refl <- testEquality (bv_width bv) sz = [bv]
    | Just LeqProof <- testLeq sz (bv_width bv) =
        let bvs2n :: [v (BVType (k+k))] -- solve for size (n+n), then split into size n
            bvs2n = withLeqProof (dblPosIsPos (LeqProof :: LeqProof 1 k)) $
              bvVectorize (addNat sz sz) bv
        in concatMap (\v -> let (a, b) = bvSplit v in [a, b]) bvs2n
  bvVectorize _ _ = error "Unhandled case"

  bvUnvectorize :: forall k n. (1 <= k) => NatRepr n -> [v (BVType k)] -> v (BVType n)
  bvUnvectorize sz [x]
    | Just Refl <- testEquality (bv_width x) sz = x
  bvUnvectorize sz bvs = withLeqProof (dblPosIsPos (LeqProof :: LeqProof 1 k)) $
      bvUnvectorize sz $ concatBVPairs bvs
    where concatBVPairs :: (1 <= o) => [v (BVType o)] -> [v (BVType (o+o))]
          concatBVPairs (x:y:zs) = (x `bvCat` y) : concatBVPairs zs
          concatBVPairs _ = []

  vectorize2 :: (1 <= k, 1 <= n)
             => NatRepr k
             -> (v (BVType k) -> v (BVType k) -> v (BVType k))
             -> v (BVType n) -> v (BVType n)
             -> v (BVType n)
  vectorize2 sz op x y = let xs = bvVectorize sz x
                             ys = bvVectorize sz y
                         in bvUnvectorize (bv_width x) $ zipWith op xs ys

  -- | Rotations
  bvRol :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)
  bvRol v n = bvShl v n .|. bvShr v bits_less_n
    where
      bits_less_n = bvSub (bvLit (bv_width v) (widthVal $ bv_width v)) n

  bvRor :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)
  bvRor v n = bvShr v n .|. bvShl v bits_less_n
    where
      bits_less_n = bvSub (bvLit (bv_width v) (widthVal $ bv_width v)) n

  -- | Shifts, the semantics is undefined for shifts >= the width of the first argument.
  --
  -- The first argument is the value to shift, and the second argument
  -- is the number of bits to shift by.
  --
  -- Big-endian, so left shift moves bits to higher-order positions
  -- and right shift moves bits to lower-order positions.
  bvShr, bvSar, bvShl :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Truncate the value.
  --
  -- Returns 'm' lower order bits.
  bvTrunc :: (1 <= m, m <= n) => NatRepr m -> v (BVType n) -> v (BVType m)
  bvTrunc w e = GHCBug.elimReflexiveTrans (bv_width e) $
    case testStrictLeq w (bv_width e) of
      Left LeqProof -> bvTrunc' w e
      Right r -> GHCBug.nonLoopingCoerce' r e

  -- | Version of 'bvTrunc' that precludes trivial truncations; see
  -- 'uext'' docs.
  bvTrunc' :: (1 <= m, m+1 <= n) => NatRepr m -> v (BVType n) -> v (BVType m)

  -- | Truncate the value.
  --
  -- Drops the 'm' low order bits.
  bvDrop :: forall m n.
    (1 <= n, 1 <= n - m, n - m <= n, m <= n) =>
    NatRepr m -> v (BVType n) -> v (BVType (n - m))
  bvDrop m v = bvTrunc (subNat n m) $ v `bvShr` (bvLit n (natValue m))
    where
      n :: NatRepr n
      n = bv_width v

  -- | Unsigned less than
  bvUlt :: v (BVType n) -> v (BVType n) -> v BoolType

  -- | Signed less than
  bvSlt :: v (BVType n) -> v (BVType n) -> v BoolType

  -- | Returns bit at index given by second argument, 0 being lsb
  bvBit :: (1 <= log_n) => v (BVType n) -> v (BVType log_n) -> v BoolType

  -- | Return most significant bit of number.
  msb :: (1 <= n) => v (BVType n) -> v BoolType
  msb v = bvSlt v (bvLit (bv_width v) (0::Integer))
--  msb v = bvBit v (bvLit (bv_width v) (widthVal (bv_width v) - 1))
     -- FIXME: should be log2 (bv_width v) here

  -- | Perform a signed extension of a bitvector.
  sext :: (1 <= m, m <= n) => NatRepr n -> v (BVType m) -> v (BVType n)
  sext w e = GHCBug.elimReflexiveTrans w $
    case testStrictLeq (bv_width e) w of
      Left LeqProof -> sext' w e
      Right r -> GHCBug.nonLoopingCoerce r e

  -- | Version of 'sext' that precludes trivial extensions; see
  -- 'uext'' docs.
  sext' :: (1 <= m, m+1 <= n) => NatRepr n -> v (BVType m) -> v (BVType n)

  -- | Perform a unsigned extension of a bitvector.
  --
  -- Unlike 'uext' below, this is a strict extension: the 'm+1 <= n'
  -- means 'm < n'.
  --
  -- We have this strict version because constructors which reify
  -- these ops, e.g. @App@ in Reopt and Crucible, are strict in this
  -- sense.
  uext' :: (1 <= m, m+1 <= n) => NatRepr n -> v (BVType m) -> v (BVType n)

  -- | Perform a unsigned extension of a bitvector.
  uext :: forall m n
        . (1 <= m, m <= n)
        => NatRepr n
        -> v (BVType m)
        -> v (BVType n)
  uext w e = GHCBug.elimReflexiveTrans w $
    case testStrictLeq (bv_width e) w of
      Left LeqProof -> uext' w e
      Right r -> GHCBug.nonLoopingCoerce r e

  -- | Return least-significant nibble (4 bits).
  least_nibble :: forall n . (4 <= n) => v (BVType n) -> v (BVType 4)
  least_nibble = bvTrunc knownNat

  -- | Return least-significant byte.
  least_byte :: forall n . (8 <= n) => v (BVType n) -> v (BVType 8)
  least_byte = bvTrunc knownNat

  -- | Return true if value contains an even number of true bits.
  even_parity :: v (BVType 8) -> v BoolType

  -- | Reverse the bytes in a bitvector expression.
  -- The parameter n should be a multiple of 8.
  reverse_bytes :: v (BVType n) -> v (BVType n)

  -- | Return true expression is signed add overflows.  See
  -- @sadc_overflows@ for definition.
  sadd_overflows :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  sadd_overflows x y = sadc_overflows x y false

  -- | Return true expression is unsigned add overflows.  See
  -- @sadc_overflows@ for definition.
  uadd_overflows :: v (BVType n) -> v (BVType n) -> v BoolType
  uadd_overflows x y = uadc_overflows x y false

  -- | Return true expression if a signed add-with carry would overflow.
  -- This holds if the sign bits of the arguments are the same, and the sign
  -- of the result is different.
  sadc_overflows :: v (BVType n) -> v (BVType n) -> v BoolType -> v BoolType

  -- | Return true expression if a unsigned add-with carry would overflow.
  uadc_overflows :: v (BVType n) -> v (BVType n) -> v BoolType -> v BoolType

  -- | Return true expression if unsigned sub overflows.
  -- @usub_overflows x y@ is true when @x - y@ (interpreted as unsigned numbers),
  -- would return a negative result.
  usub_overflows :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  usub_overflows x y = usbb_overflows x y false

  -- | Return true expression is signed sub overflows.
  ssub_overflows :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  ssub_overflows x y = ssbb_overflows x y false

  -- | Return true expression if unsigned sbb overflows.
  -- @usbb_overflows x y c@ is true when @x - (y+c)@ with
  -- x,y interpreted as unsigned numbers and c a borrow bit,
  -- would return a negative result.
  usbb_overflows :: (1 <= n)
                 => v (BVType n)
                 -> v (BVType n)
                 -> v BoolType
                 -> v BoolType

  -- | Return true expression if unsigned sub overflows.
  -- @ssbb_overflows x y c@ is true when @x - (y+c)@ with
  -- x,y interpreted as signed numbers and c a borrow bit,
  -- would return a number different from the expected integer due to
  -- wrap-around.
  ssbb_overflows :: (1 <= n)
                 => v (BVType n)
                 -> v (BVType n)
                 -> v BoolType
                 -> v BoolType

  -- | bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsf :: v (BVType n) -> v (BVType n)

  -- | bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices greater than return value must be unset.
  bsr :: v (BVType n) -> v (BVType n)

  -- FP stuff

  -- | is NaN (quiet and signalling)
  isQNaN :: FloatInfoRepr flt -> v (FloatType flt) -> v BoolType
  isSNaN :: FloatInfoRepr flt -> v (FloatType flt) -> v BoolType

  isNaN :: FloatInfoRepr flt -> v (FloatType flt) -> v BoolType
  isNaN fir v = isQNaN fir v .|. isSNaN fir v

  -- | Add two floating point numbers.
  -- This can throw exceptions, including:
  -- #IA Operand is an SNaN value or unsupported format.
  --     Operands are infinities of unlike sign.
  -- #D  Source operand is a denormal value.
  -- #U  Result is too small for destination format.
  -- #O  Result is too large for destination format.
  -- #P  Value cannot be represented exactly in destination format.
  fpAdd :: FloatInfoRepr flt
        -> v (FloatType flt)
        -> v (FloatType flt)
        -> v (FloatType flt)

  -- | Indicates whether floating point representation from addition is
  -- larger than the actual value.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpAddRoundedUp :: FloatInfoRepr flt
                 -> v (FloatType flt)
                 -> v (FloatType flt)
                 -> v BoolType

  -- | Subtracts one floating point numbers from another.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpSub :: FloatInfoRepr flt
        -> v (FloatType flt)
        -> v (FloatType flt)
        -> v (FloatType flt)

  -- | Indicates whether floating point representation from subtraction
  -- is larger than the actual value.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpSubRoundedUp :: FloatInfoRepr flt
                 -> v (FloatType flt)
                 -> v (FloatType flt)
                 -> v BoolType

  -- | Multiplies two floating point numbers.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpMul :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v (FloatType flt)

  -- | Whether the result of the mul was rounded up
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpMulRoundedUp :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v BoolType

  -- | Divides two floating point numbers.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpDiv :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v (FloatType flt)

  -- | Compare if one floating is strictly less than another.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpLt :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v BoolType

  -- | Floating point equality (equates -0 and 0)
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpEq :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v BoolType
  fpEq fir v v' = complement (fpLt fir v v' .|. fpLt fir v' v)

  -- | Convert between floating point values
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpCvt :: FloatInfoRepr flt -> FloatInfoRepr flt' -> v (FloatType flt) -> v (FloatType flt')

  -- | Whether roundup occurs when converting between FP formats
  fpCvtRoundsUp :: FloatInfoRepr flt -> FloatInfoRepr flt' -> v (FloatType flt) -> v BoolType

  -- | Convert a signed integer to a float. (e.g. 255 -> 255.0)
  -- (see x86_64 CVTSI2SD instruction)
  -- We assume that the floating point representation is large enough to hold
  -- all the values at that bitwidth.
  fpFromBV :: FloatInfoRepr flt -> v (BVType n) -> v (FloatType flt)

  -- | Convert a floating point value to a signed integer.
  -- * If the conversion is inexact, then the value is truncated to zero.
  -- * If the conversion is out of the range of the bitvector, then a
  --   floating point exception should be raised.
  -- * If that exception is masked, then this returns -1 (as a signed bitvector).
  truncFPToSignedBV :: NatRepr n
                    -> FloatInfoRepr flt
                    -> v (FloatType flt)
                    -> v (BVType n)


  true :: v BoolType
  true = bvLit knownNat (1::Integer)

  false :: v BoolType
  false = bvLit knownNat (0::Integer)

  boolValue :: Bool -> v BoolType
  boolValue b = if b then true else false

-- | Construct a literal bit vector.  The result is undefined if the
-- literal does not fit withint the given number of bits.
bvKLit :: (IsValue v, KnownNat n, 1 <= n) => Integer -> v (BVType n)
bvKLit = bvLit knownNat

-- | Add two bitvectors together dropping overflow.
(.+) :: (1 <= n, IsValue v) => v (BVType n) -> v (BVType n) -> v (BVType n)
(.+) = bvAdd

-- | Subtract two vectors, ignoring underflow.
(.-) :: (1 <= n, IsValue v) => v (BVType n) -> v (BVType n) -> v (BVType n)
(.-) = bvSub

-- | Performs a multiplication of two bitvector values.
(.*) :: (1 <= n, IsValue v) => v (BVType n) -> v (BVType n) -> v (BVType n)
(.*) = bvMul

infixl 7 .*
infixl 6 .+
infixl 6 .-
infix  4 .=

------------------------------------------------------------------------
-- Monadic definition

-- | This returns the type associated with values that can be read
-- for the semantics monad.
type family Value (m :: * -> *) :: Type -> *

type Pred m = Value m BoolType

type MLocation m = Location (Value m (BVType 64))

data ExceptionClass
   = DivideError -- #DE
   | FloatingPointError
   | SIMDFloatingPointException
   | GeneralProtectionException Int
     -- ^ A general protection exception with the given error code.
     -- -- | AlignmentCheck
  deriving (Eq, Ord, Show)

-- | Primitive instructions.
--
-- A primitive instruction is one whose semantics depend on the
-- underlying hardware or OS.
data Primitive
   = Syscall
   | CPUID
   | RDTSC
   -- | The semantics of @xgetbv@ seems to depend on the semantics of @cpuid@.
   | XGetBV
   deriving (Eq, Ord, Show)

ppPrimitive :: Primitive -> Doc
ppPrimitive = text . map toLower . show

instance Pretty Primitive where
  pretty = ppPrimitive

-- | The Semantics Monad defines all the operations needed for the x86
-- semantics.
class ( Applicative m
      , Monad m
      , IsValue (Value m)
      ) => Semantics m where

  -- | Create a fresh "undefined" value.
  make_undefined :: TypeRepr tp -> m (Value m tp)

  -- | Mark a Boolean variable as undefined.
  set_undefined :: KnownType tp => MLocation m tp -> m ()
  set_undefined l = do
    u <- make_undefined knownType
    l .= u

  -- | Read from the given location.
  get :: MLocation m tp -> m (Value m tp)
  -- | Assign a value to alocation.
  (.=) :: MLocation m tp -> Value m tp -> m ()

  -- | Modify the value at a location
  modify :: (Value m tp -> Value m tp) -> MLocation m tp -> m ()
  modify f r = do
    x <- get r
    r .= f x

  -- | Perform an if-then-else
  ifte_ :: Value m BoolType -> m () -> m () -> m ()

  -- | Run a step if condition holds.
  when_ :: Value m BoolType -> m () -> m ()
  when_ p x = ifte_ p x (return ())

  -- | Run a step if condition is false.
  unless_ :: Value m BoolType -> m () -> m ()
  unless_ p x = ifte_ p (return ()) x

  -- FIXME: use location instead?
  -- | Move n bits at a time, with count moves
  --
  -- Semantic sketch. The effect on memory should be like @memcopy@
  -- below, not like @memcopy2@. These sketches ignore the issue of
  -- copying in chunks of size `bytes`, which should only be an
  -- efficiency concern.
  --
  -- @
  -- void memcopy(int bytes, int copies, char *src, char *dst, int reversed) {
  --   int maybeFlip = reversed ? -1 : 1;
  --   for (int c = 0; c < copies; ++c) {
  --     for (int b = 0; b < bytes; ++b) {
  --       int offset = maybeFlip * (b + c * bytes);
  --       *(dst + offset) = *(src + offset);
  --     }
  --   }
  -- }
  -- @
  --
  -- Compare with:
  --
  -- @
  -- void memcopy2(int bytes, int copies, char *src, char *dst, int reversed) {
  --   int maybeFlip = reversed ? -1 : 1;
  --   /* The only difference from `memcopy` above: here the same memory is
  --      copied whether `reversed` is true or false -- only the order of
  --      copies changes -- whereas above different memory is copied for
  --      each direction. */
  --   if (reversed) {
  --     /* Start at the end and work backwards. */
  --     src += copies * bytes - 1;
  --     dst += copies * bytes - 1;
  --   }
  --   for (int c = 0; c < copies; ++c) {
  --     for (int b = 0; b < bytes; ++b) {
  --       int offset = maybeFlip * (b + c * bytes);
  --       *(dst + offset) = *(src + offset);
  --     }
  --   }
  -- }
  -- @
  memcopy :: Integer
             -- ^ Number of bytes to copy at a time (1,2,4,8)
          -> Value m (BVType 64)
             -- ^ Number of values to move.
          -> Value m (BVType 64)
             -- ^ Start of source buffer
          -> Value m (BVType 64)
             -- ^ Start of destination buffer.
          -> Value m BoolType
             -- ^ Flag indicates direction of move:
             -- True means we should decrement buffer pointers after each copy.
             -- False means we should increment the buffer pointers after each copy.
          -> m ()

  -- | Compare the memory regions.  Returns the number of elements which are
  -- identical.  If the direction is 0 then it is increasing, otherwise decreasing.
  --
  -- See `memcopy` above for explanation of which memory regions are
  -- compared: the regions copied there are compared here.
  memcmp :: Integer
            -- ^ Number of bytes to compare at a time {1, 2, 4, 8}
            -> Value m (BVType 64)
            -- ^ Number of elementes to compare
            -> Value m (BVType 64)
            -- ^ Pointer to first buffer
            -> Value m (BVType 64)
            -- ^ Pointer to second buffer
            -> Value m BoolType
             -- ^ Flag indicates direction of copy:
             -- True means we should decrement buffer pointers after each copy.
             -- False means we should increment the buffer pointers after each copy.
            -> m (Value m (BVType 64))

  -- | Set memory to the given value, for the number of words (nbytes
  -- = count * bv_width v)
  memset :: Value m (BVType 64)
            -- ^ Number of values to set
            -> Value m (BVType n)
            -- ^ Value to set
            -> Value m (BVType 64)
            -- ^ Pointer to buffer to set
            -> Value m (BVType 1)
            -- ^ Direction flag
            -> m ()

  -- | Compare the contents of a memory region against a value.  Returns the number of elements which are
  -- identical (resp. different).  If the direction is 0 then it is
  -- increasing, otherwise decreasing.
  find_element :: (1 <= n) =>
                  Integer
                  -- ^ Number of bytes to compare at a time {1, 2, 4, 8}
                  -> Bool
                  -- ^ Find first matching (True) or not matching (False)
                  -> Value m (BVType 64)
                  -- ^ Number of elementes to compare
                  -> Value m (BVType 64)
                  -- ^ Pointer to first buffer
                  -> Value m (BVType n)
                  -- ^ Value to compare
                  -> Value m BoolType
                  -- ^ Flag indicates direction of copy:
                  -- True means we should decrement buffer pointers after each copy.
                  -- False means we should increment the buffer pointers after each copy.
                  -> m (Value m (BVType 64))

  -- | execute a primitive instruction.
  primitive :: Primitive -> m ()

  -- | Return the base address of the given segment.
  getSegmentBase :: Segment -> m (Value m (BVType 64))

  -- | Unsigned division.
  --
  -- Rounds the result to zero.  Undefined if the denominator is zero.
  --
  -- This and the other BV division operations ('bvRem',
  -- 'bvSignedQuot', 'bvSignedRem') are defined in 'Semantics', and
  -- not in 'IsValue', because they are inherently partial operations.
  bvQuot :: (1 <= n)
         => Value m (BVType n)
         -> Value m (BVType n)
         -> m (Value m (BVType n))

  -- | Unsigned remainder.
  --
  -- Rounds the result to zero.  Undefined if the denominator is zero.
  bvRem :: (1 <= n)
        => Value m (BVType n)
        -> Value m (BVType n)
        -> m (Value m (BVType n))

  -- | Signed division.
  --
  -- Rounds the result to zero.  Undefined if the denominator is zero,
  -- or if the result overflows (dividing the largest negative number
  -- by @-1@ causes this overflow).
  --
  -- The x86 documentation for @idiv@ (Intel x86 manual volume 2A,
  -- page 3-393) says that results should be rounded towards
  -- zero. These operations are called @quot@ and @rem@ in Haskell,
  -- whereas @div@ and @mod@ in Haskell round towards negative
  -- infinity.
  bvSignedQuot :: (1 <= n)
               => Value m (BVType n)
               -> Value m (BVType n)
               -> m (Value m (BVType n))

  -- | Signed remainder.
  --
  -- Rounds the result to zero.  Undefined if the denominator is zero.
  bvSignedRem :: (1 <= n)
              => Value m (BVType n)
              -> Value m (BVType n)
              -> m (Value m (BVType n))

  -- | raises an exception if the predicate is true and the mask is false
  exception :: Value m BoolType    -- mask
            -> Value m BoolType -- predicate
            -> ExceptionClass
            -> m ()

  -- FIXME: those should also mutate the underflow/overflow flag and
  -- related state.

  -- | X87 support --- these both affect the register stack for the
  -- x87 state.
  x87Push :: Value m (FloatType X86_80Float) -> m ()
  x87Pop  :: m ()

-- | Defines operations that need to be supported at a specific bitwidht.
type SupportedBVWidth n
   = ( IsLeq 1 n
     , IsLeq 4 n
     , IsLeq 8 n
     , KnownNat n
     )

-- | @IsLocationBV m n@ is a constraint used to indicate that @m@
-- implements Semantics, and @Value m (BV n)@ supports the operations
-- used to assign registers.
type IsLocationBV m n
   = ( SupportedBVWidth n
     , Semantics m
     )

-- | @This defines all the constraint that must be implemented to support
-- interpreting x86 instructions.
type FullSemantics m
   = ( Semantics m
     )
