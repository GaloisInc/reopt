{-
Copyright        : (c) Galois, Inc 2015-2017
Maintainer       : Joe Hendrix <jhendrix@galois.com>, Simon Winwood <sjw@galois.com>

This defines the X86_64 architecture type and the supporting definitions.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Reopt.Machine.X86State
  ( -- * Architecture
    X86_64
  , X86PrimFn(..)
  , X86PrimLoc(..)
  , X86Stmt(..)
  ) where

import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Flexdis86 as F
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Macaw.CFG
import           Data.Macaw.Fold
import           Data.Macaw.Types

import           Data.Macaw.X86.Monad (SIMDWidth(..), RepValSize(..))
import           Data.Macaw.X86.X86Reg
import           Data.Macaw.X86.X87ControlReg

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
      PShufb w _ _  -> BVTypeRepr (typeRepr w)
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
