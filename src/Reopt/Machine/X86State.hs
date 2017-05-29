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
    -- * Combinators
  , foldValueCached
    -- * X86Reg
  , X86Reg(..)
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
  , refsInX86PrimFn
  , x86CalleeSavedRegs
  , x86ArgumentRegs
  , x86FloatArgumentRegs
  , x86ResultRegs
  , x86FloatResultRegs
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Flexdis86 as F
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Macaw.CFG
import           Data.Macaw.Memory ( MemWord, addrValue )
import           Data.Macaw.Types
import           Data.Macaw.X86.X86Reg
import           Data.Macaw.X86.X87ControlReg

import           Reopt.Semantics.Monad (SIMDWidth(..), RepValSize(..))

------------------------------------------------------------------------
-- X86_64 specific declarations

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
   | forall w . (tp ~ BVType   w) => X87_ControlLoc !(X87_ControlReg w)
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
  prettyF (MemSet cnt val dest d) =
      text "memset" <+> parens (hcat $ punctuate comma args)
    where args = [pretty cnt, pretty val, pretty dest, pretty d]

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
                 & boundValue X87_TopReg .~ mkLit knownNat (toInteger (loc_x87_top loc))
                 & boundValue df .~ mkLit knownNat (if (loc_df_flag loc) then 1 else 0)

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
refsInX86Stmt (MemCopy _ cnt src dest d) =
  Set.unions [ refsInValue cnt
             , refsInValue src
             , refsInValue dest
             , refsInValue d
             ]
refsInX86Stmt (MemSet cnt val dest d) =
  Set.unions [ refsInValue cnt
             , refsInValue val
             , refsInValue dest
             , refsInValue d
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
  [ -- Some rsp sjw: rsp is special
    Some rbp
  , Some rbx
  , Some r12
  , Some r13
  , Some r14
  , Some r15
  , Some X87_TopReg
  , Some df
  ]

x86ArgumentRegs :: [X86Reg (BVType 64)]
x86ArgumentRegs = [rdi, rsi, rdx, rcx, r8, r9]

x86FloatArgumentRegs :: [X86Reg (BVType 128)]
x86FloatArgumentRegs =  X86_XMMReg <$> [0..7]

x86ResultRegs :: [X86Reg (BVType 64)]
x86ResultRegs = [ rax, rdx ]

x86FloatResultRegs :: [X86Reg (BVType 128)]
x86FloatResultRegs = [ X86_XMMReg 0 ]

-- | How the stack pointer moves when a call is made.
x86StackDelta :: Integer
x86StackDelta = -8
