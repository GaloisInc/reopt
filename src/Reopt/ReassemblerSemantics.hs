{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Semantics where

import Control.Applicative
import Data.Bits
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Numeric
import Text.PrettyPrint.Leijen as PP hiding ((<$>))

import Reassembler.InstructionSet

commas :: [Doc] -> Doc
commas = foldr (\x y -> x <> char ',' <+> y) PP.empty

------------------------------------------------------------------------
-- Addr

newtype Addr = Addr { unAddr :: Word64 }
  deriving (Eq, Ord)

instance Show Addr where
  show a | l < 16 = replicate (16-l) '0' ++ s
         | otherwise = s
    where s = showHex (unAddr a) ""
          l = length s

------------------------------------------------------------------------
-- Bit

newtype Bit = Bit Bool

instance Num Bit where
  Bit x + Bit y = Bit (x /= y)
  Bit x * Bit y = Bit (x && y)
  abs x = x
  signum x = x
  fromInteger i = Bit (i `testBit` 0)

------------------------------------------------------------------------
-- RFLAGS

-- | Indicates a specific bit in the RFLAGS register
-- Valid indices range from 0 to 63.
newtype RFLAGS = RFLAGS Word8

cf :: RFLAGS
cf = RFLAGS 0 

pf :: RFLAGS 
pf = RFLAGS 2

af :: RFLAGS 
af = RFLAGS 4

zf :: RFLAGS
zf = RFLAGS 6

sf :: RFLAGS
sf = RFLAGS 7

tf :: RFLAGS
tf = RFLAGS 8

if_rflag :: RFLAGS
if_rflag = RFLAGS 9

df :: RFLAGS
df = RFLAGS 10

of_rflag :: RFLAGS
of_rflag = RFLAGS 11

------------------------------------------------------------------------
-- Cond

data Cond
   = And [Cond]
   | Or [Cond]
   | EqCond Cond Cond
   | Not Cond
   | RFLAGS_Set RFLAGS

is_set :: RFLAGS -> Cond
is_set r = RFLAGS_Set r

is_unset :: RFLAGS -> Cond
is_unset r = Not (RFLAGS_Set r)

class SemEq t u where
  (.==) :: t -> u -> Cond
  (./=) :: t -> u -> Cond
  x ./= y = Not (x .== y)

instance SemEq RFLAGS Bit where
  r .== Bit True  = RFLAGS_Set r
  r .== Bit False = Not (RFLAGS_Set r)

instance SemEq RFLAGS RFLAGS where
  x .== y  = EqCond (RFLAGS_Set x) (RFLAGS_Set y)

------------------------------------------------------------------------
-- TypeOf

data TypeOf tp where
  Type8  :: TypeOf Word8
  Type16 :: TypeOf Word16
  Type32 :: TypeOf Word32
  Type64 :: TypeOf Word64

instance Show (TypeOf tp) where
  show Type8  = "Type8"
  show Type16 = "Type16"
  show Type32 = "Type32"
  show Type64 = "Type64"

-- Width of argument in bytes.
bytew :: TypeOf tp -> Integer
bytew Type8  = 1
bytew Type16 = 2
bytew Type32 = 4
bytew Type64 = 8

-- Width of argument in bits
bitw :: TypeOf tp -> Integer
bitw tp = 8 * bytew tp

class (Bits w, Integral w) => TypeOfType w where
  typeOf :: w -> TypeOf w

instance TypeOfType Word8 where
  typeOf _ = Type8

instance TypeOfType Word16 where
  typeOf _ = Type16

instance TypeOfType Word32 where
  typeOf _ = Type32

instance TypeOfType Word64 where
  typeOf _ = Type64

------------------------------------------------------------------------
-- ValueF

data ValueF c (f :: * -> *) tp where
  CnsValue8  :: Word8  -> ValueF c f Word8
  CnsValue16 :: Word16 -> ValueF c f Word16
  CnsValue32 :: Word32 -> ValueF c f Word32
  CnsValue64 :: Word64 -> ValueF c f Word64
  IteValue   :: c -> f tp -> f tp -> ValueF c f tp

  AddValue     :: f tp -> f tp -> ValueF c f tp
  SubValue     :: f tp -> f tp -> ValueF c f tp
  -- | Negate value in two's complement.
  NegValue     :: f tp -> ValueF c f tp
  -- | @MulValueLow x y@ returns value of @(x * y) mod 2^sz@ where
  -- @sz@ is the size of the type.
  MulValueLow  :: f tp -> f tp -> ValueF c f tp
  -- | @MulValueHigh x y@ returns value of @(x * y) div 2^sz@ where
  -- @sz@ is the size of the type.  The operands are unsigned.
  MulValueHigh :: TypeOf tp -> f tp -> f tp -> ValueF c f tp
  -- @DivValue x y z@ denotes @x:y `div` z@.
  DivValue     :: TypeOf tp -> f tp -> f tp -> f tp -> ValueF c f tp
  -- @ModValue x y z@ denotes @x:y `mod` z@.
  ModValue     :: TypeOf tp -> f tp -> f tp -> f tp -> ValueF c f tp

  -- Bitwise ops

  -- Complement each bit.
  NotValue :: f tp -> ValueF c f tp  
  -- Bitwise and of arguments.
  AndValue :: f tp -> f tp -> ValueF c f tp
  -- Bitwise or of arguments.
  OrValue  :: f tp -> f tp -> ValueF c f tp
  -- Bitwise xor of arguments.
  XorValue :: f tp -> f tp -> ValueF c f tp
  -- @ShlValue x y@ returns @x@ with bits shifted @y@ places to left.
  -- @y@ is treated as unsigned, and has had its low-order bits masked.
  ShlValue :: f tp -> f Word8 -> ValueF c f tp
  -- @ShrValue x y@ returns @x@ with bits shifted @y@ places to right.
  -- This is a logical shift, so the new valeus are zeros.
  -- @y@ is treated as unsigned, and has had its low-order bits masked.
  ShrValue :: f tp -> f Word8 -> ValueF c f tp

  -- | Return the index of the least-significant bit that is 1, or
  -- undefined if the input is 0.
  LeastSignificantOne :: f tp -> ValueF c f tp

  Trunc16To8  :: f Word16 -> ValueF c f Word8
  Trunc32To8  :: f Word32 -> ValueF c f Word8
  Trunc32To16 :: f Word32 -> ValueF c f Word16
  Trunc64To8  :: f Word64 -> ValueF c f Word8
  Trunc64To16 :: f Word64 -> ValueF c f Word16
  Trunc64To32 :: f Word64 -> ValueF c f Word32

  -- | Zero extend a 1-bit value to 8 bits.
  ZExt1To8  :: c -> ValueF c f Word8
  -- | Zero extend 8-bit value to 16 bits.
  ZExt8To16  :: f Word8  -> ValueF c f Word16
  -- | Zero extend 8-bit value to 32 bits.
  ZExt8To32  :: f Word8  -> ValueF c f Word32
  -- | Zero extend 8-bit value to 64 bits.
  ZExt8To64  :: f Word8  -> ValueF c f Word64
  -- | Zero extend 16-bit value to 32 bits.
  ZExt16To32 :: f Word16 -> ValueF c f Word32
  -- | Zero extend 16-bit value to 64 bits.
  ZExt16To64 :: f Word16 -> ValueF c f Word64
  -- | Zero extend 32-bit value to 64 bits.
  ZExt32To64 :: f Word32 -> ValueF c f Word64

  -- | Sign extend 32-bit value to 64 bits.
  SExt32To64  :: f Word32 -> ValueF c f Word64

------------------------------------------------------------------------
-- traverseValueF 

traverseValueF :: (Applicative m, TypeOfType tp)
               => (c -> m d)
               -> (forall tp' . TypeOfType tp' => f tp' -> m (g tp'))
               -> ValueF c f tp
               -> m (ValueF d g tp)
traverseValueF evalc evalf vf =
  case vf of
    CnsValue8 x  -> pure $ CnsValue8  x
    CnsValue16 x -> pure $ CnsValue16 x
    CnsValue32 x -> pure $ CnsValue32 x
    CnsValue64 x -> pure $ CnsValue64 x
    IteValue c x y -> IteValue <$> evalc c <*> evalf x <*> evalf y

    AddValue x y -> AddValue <$> evalf x <*> evalf y
    SubValue x y -> SubValue <$> evalf x <*> evalf y
    NegValue x   -> NegValue <$> evalf x
    MulValueLow  x y -> MulValueLow  <$> evalf x <*> evalf y
    MulValueHigh tp x y -> MulValueHigh tp <$> evalf x <*> evalf y
    DivValue tp x y z -> DivValue tp <$> evalf x <*> evalf y <*> evalf z
    ModValue tp x y z -> ModValue tp <$> evalf x <*> evalf y <*> evalf z

    NotValue x   -> NotValue <$> evalf x
    AndValue x y -> AndValue <$> evalf x <*> evalf y
    OrValue x y  -> OrValue  <$> evalf x <*> evalf y
    XorValue x y -> XorValue <$> evalf x <*> evalf y
    ShlValue x y -> ShlValue <$> evalf x <*> evalf y
    ShrValue x y -> ShrValue <$> evalf x <*> evalf y

    LeastSignificantOne x -> LeastSignificantOne <$> evalf x

    Trunc16To8  x -> Trunc16To8  <$> evalf x
    Trunc32To8  x -> Trunc32To8  <$> evalf x
    Trunc32To16 x -> Trunc32To16 <$> evalf x
    Trunc64To8  x -> Trunc64To8  <$> evalf x
    Trunc64To16 x -> Trunc64To16 <$> evalf x
    Trunc64To32 x -> Trunc64To32 <$> evalf x

    ZExt1To8   x -> ZExt1To8   <$> evalc x
    ZExt8To16  x -> ZExt8To16  <$> evalf x
    ZExt8To32  x -> ZExt8To32  <$> evalf x
    ZExt8To64  x -> ZExt8To64  <$> evalf x
    ZExt16To32 x -> ZExt16To32 <$> evalf x
    ZExt16To64 x -> ZExt16To64 <$> evalf x
    ZExt32To64 x -> ZExt32To64 <$> evalf x

    SExt32To64 x -> SExt32To64 <$> evalf x

------------------------------------------------------------------------
-- Value1 and ValueOf

type family V1  (m :: * -> *)
type family V8  (m :: * -> *)
type family V16 (m :: * -> *)
type family V32 (m :: * -> *)
type family V64 (m :: * -> *)

data Value1 m where
  IsZero  :: ValueOf m tp -> Value1 m

  -- @BitSet x i@ holds if bit @i@ at @x@ if value.
  -- Value is undefined if @i@ is out of range for tp.
  BitSet :: ValueOf m tp -> ValueOf m Word8 -> Value1 m

  -- | Holds if two values are equal.   
  IsEq :: ValueOf m tp -> ValueOf m tp -> Value1 m
  
  SAddOverflow :: ValueOf m tp -> ValueOf m tp -> Value1 m
  UAddOverflow :: ValueOf m tp -> ValueOf m tp -> Value1 m
  -- | Holds if adding lowest 4-bits of each word will overflow.
  UAdd4_Overflows :: ValueOf m Word8 -> ValueOf m Word8 -> Value1 m

  -- | Holds if a signed subtraction of the two arguments overflows.
  SSub_Overflows :: ValueOf m tp -> ValueOf m tp -> Value1 m
  -- | Holds if an unsigned subtraction of the two arguments overflows.
  USub_Overflows :: ValueOf m tp -> ValueOf m tp -> Value1 m
  -- | Holds if a subtraction of lowest 4-bits of each byte will overflow.
  USub4_Overflows :: ValueOf m Word8 -> ValueOf m Word8 -> Value1 m

  -- | Holds if signed multiplciation of two operands overflows a
  -- register with the given size.
  STimes_Overflows :: ValueOf m tp -> ValueOf m tp -> Value1 m

  -- | @Div_Overflows x y z@ returns true if @x:y/z@ would overflow the
  -- quotient register.
  Div_Overflows :: ValueOf m tp -> ValueOf m tp -> ValueOf m tp -> Value1 m

  -- | Holds if value contains an even number of 1bits.
  EvenParity :: ValueOf m Word8 -> Value1 m

  CnsValue1 :: Bool -> Value1 m

  UndefValue1 :: Value1 m

  ExtValue1 :: V1 m -> Value1 m

  AndValue1 :: [Value1 m] -> Value1 m
  OrValue1  :: [Value1 m] -> Value1 m
  EqValue1  :: Value1 m -> Value1 m -> Value1 m
  NotValue1 :: Value1 m -> Value1 m

  -- @MuxValue1 c t f@ returns @t@ if @c@ is set and @f@ if not.
  MuxValue1  :: Value1 m -> Value1 m -> Value1 m -> Value1 m

data ValueOf (m :: * -> *) tp where
  VF  :: ValueF (Value1 m) (ValueOf m) tp -> ValueOf m tp

  ExtValue8  :: V8 m   -> ValueOf m Word8
  ExtValue16 :: V16 m  -> ValueOf m Word16
  ExtValue32 :: V32 m  -> ValueOf m Word32
  ExtValue64 :: V64 m  -> ValueOf m Word64


xor_value1 :: Value1 m -> Value1 m -> Value1 m
xor_value1 x y = NotValue1 (EqValue1 x y)

-- | Return predicate checking if msb is set.
msb :: ValueOf m tp -> Value1 m
msb v = v `BitSet` cnsValue Type8 (bitw tp - 1)
  where tp = typeOfOperand v
is_zero :: ValueOf m tp -> Value1 m
is_zero = IsZero

uadd4_overflows :: ValueOf m Word8 -> ValueOf m Word8 -> Value1 m
uadd4_overflows = UAdd4_Overflows

uadd_overflows :: ValueOf m tp -> ValueOf m tp -> Value1 m
uadd_overflows = UAddOverflow

sadd_overflows :: ValueOf m tp -> ValueOf m tp -> Value1 m
sadd_overflows = SAddOverflow

even_parity :: ValueOf m Word8 -> Value1 m
even_parity = EvenParity

(.+) :: ValueOf m tp -> ValueOf m tp -> ValueOf m tp
x .+ y = VF (AddValue x y)

(.-) :: ValueOf m tp -> ValueOf m tp -> ValueOf m tp
x .- y = VF (SubValue x y)

negV :: ValueOf m tp -> ValueOf m tp
negV x = VF (NegValue x)

mul_low :: ValueOf m tp -> ValueOf m tp -> ValueOf m tp
mul_low x y = VF (MulValueLow x y)

mul_high :: ValueOf m tp -> ValueOf m tp -> ValueOf m tp
mul_high x y = VF (MulValueHigh (typeOfOperand x) x y)

valueOfUnimplemented :: String -> a
valueOfUnimplemented nm = error $ "internal: ValueOf." ++ show nm ++ " unimplemented."
 
instance Eq (ValueOf m tp) where
  (==) = valueOfUnimplemented "(==)"

instance Bits (ValueOf m tp) where
  x .&. y = VF $ AndValue x y
  x .|. y = VF $ OrValue x y
  xor x y = VF $ XorValue x y
  complement x = VF $ NotValue x
  shift x i | i > 0     = VF $ ShlValue x (cnsValue Type8 i')
            | i == 0    = x
            | otherwise = VF $ ShrValue x (cnsValue Type8 (negate i'))
    where i' = toInteger i
  rotate   = valueOfUnimplemented "rotate"
  bit      = valueOfUnimplemented "bit"
  testBit  = valueOfUnimplemented "testBit"
  bitSize  = valueOfUnimplemented "bitSize"
  isSigned = valueOfUnimplemented "isSigned"
  popCount = valueOfUnimplemented "popCount"
  
-- | @zeroTop32 v@ returns @v .&. (^32-1)@.
zeroTop32 :: ValueOf m Word64 -> ValueOf m Word64
zeroTop32 = VF . ZExt32To64 . VF . Trunc64To32


-- | Truncate a 64-bit value to the given bitwidth.
trunc :: TypeOf stp -> ValueOf m stp -> TypeOf dtp -> ValueOf m dtp
trunc Type8  v Type8  = v
trunc Type16 v Type8  = VF $ Trunc16To8  v
trunc Type16 v Type16 = v
trunc Type32 v Type8  = VF $ Trunc32To8  v
trunc Type32 v Type16 = VF $ Trunc32To16 v
trunc Type32 v Type32 = v
trunc Type64 v Type8  = VF $ Trunc64To8  v
trunc Type64 v Type16 = VF $ Trunc64To16 v
trunc Type64 v Type32 = VF $ Trunc64To32 v
trunc Type64 v Type64 = v
trunc stp _ dtp = error $ 
  "illegal args to trunc: source type = " ++ show stp ++ ", dest type = " ++ show dtp

least_byte :: ValueOf m tp -> ValueOf m Word8
least_byte v = trunc (typeOfOperand v) v Type8

-- | Truncate a 64-bit value to the given bitwidth.
trunc64To :: ValueOf m Word64 -> TypeOf tp -> ValueOf m tp
trunc64To = trunc Type64

zext :: TypeOf stp -> ValueOf m stp -> TypeOf dtp -> ValueOf m dtp
zext Type8  v Type8  = v
zext Type8  v Type16 = VF $ ZExt8To16 v
zext Type8  v Type32 = VF $ ZExt8To32 v
zext Type8  v Type64 = VF $ ZExt8To64 v
zext Type16 v Type16 = v
zext Type16 v Type32 = VF $ ZExt16To32 v
zext Type16 v Type64 = VF $ ZExt16To64 v
zext Type32 v Type32 = v
zext Type32 v Type64 = VF $ ZExt32To64 v
zext Type64 v Type64 = v

zext stp _ dtp = error $ 
  "illegal args to zext: source type = " ++ show stp ++ ", dest type = " ++ show dtp

cnsValue :: TypeOf tp -> Integer -> ValueOf m tp
cnsValue Type8  i = VF $ CnsValue8  (fromInteger i)
cnsValue Type16 i = VF $ CnsValue16 (fromInteger i)
cnsValue Type32 i = VF $ CnsValue32 (fromInteger i)
cnsValue Type64 i = VF $ CnsValue64 (fromInteger i)

bit_at :: ValueOf m tp -> ValueOf m Word8 -> Value1 m
bit_at = BitSet

------------------------------------------------------------------------
-- typeOfOperand

typeOfOperand :: ValueOf m tp -> TypeOf tp
typeOfOperand (VF vf) =
  case vf of
    CnsValue8{}  -> Type8
    CnsValue16{} -> Type16
    CnsValue32{} -> Type32
    CnsValue64{} -> Type64
    IteValue _ x _ -> typeOfOperand x

    AddValue x _     -> typeOfOperand x
    SubValue x _     -> typeOfOperand x
    NegValue x       -> typeOfOperand x
    MulValueLow  x _ -> typeOfOperand x
    MulValueHigh tp _ _ -> tp
    DivValue tp _ _ _   -> tp
    ModValue tp _ _ _   -> tp

    NotValue x   -> typeOfOperand x
    AndValue x _ -> typeOfOperand x
    OrValue x _  -> typeOfOperand x
    XorValue x _ -> typeOfOperand x
    ShlValue x _ -> typeOfOperand x
    ShrValue x _ -> typeOfOperand x
    LeastSignificantOne x -> typeOfOperand x

    Trunc16To8{}  -> Type8
    Trunc32To8{}  -> Type8
    Trunc32To16{} -> Type16
    Trunc64To8{}  -> Type8
    Trunc64To16{} -> Type16
    Trunc64To32{} -> Type32

    ZExt1To8{}   -> Type8
    ZExt8To16{}  -> Type16
    ZExt8To32{}  -> Type32
    ZExt8To64{}  -> Type64
    ZExt16To32{} -> Type32
    ZExt16To64{} -> Type64
    ZExt32To64{} -> Type64
    SExt32To64{} -> Type64

typeOfOperand ExtValue8{}  = Type8
typeOfOperand ExtValue16{} = Type16
typeOfOperand ExtValue32{} = Type32
typeOfOperand ExtValue64{} = Type64


------------------------------------------------------------------------
-- Pretty Print values

data PrimPrinter m
   = PrimPrinter { ppV1  :: V1 m -> Doc
                 , ppV8  :: V8  m -> Doc
                 , ppV16 :: V16 m -> Doc
                 , ppV32 :: V32 m -> Doc
                 , ppV64 :: V64 m -> Doc
                 }

ppValue1 :: PrimPrinter m -> Value1 m -> Doc
ppValue1 _ _ = text "<some value1>"

ppValueOf :: forall m tp . PrimPrinter m -> ValueOf m tp -> Doc
ppValueOf pp = ppv
  where pp1 = ppValue1 pp
        ppv :: ValueOf m tp' -> Doc
        ppv v0 =
          case v0 of 
            VF vf ->
              case vf of
                CnsValue8  w -> text (showHex w "")
                CnsValue16 w -> text (showHex w "")
                CnsValue32 w -> text (showHex w "")
                CnsValue64 w -> text (showHex w "")

                IteValue c t f -> text "ite" <> parens (commas [pp1 c, ppv t, ppv f])

                AddValue x y -> ppv x <+> text "+" <+> ppv y
                SubValue x y  -> ppv x <+> text "-" <+> ppv y
                NegValue x    -> text "-" <+> ppv x
                MulValueLow  x y -> text "low"  <> parens (ppv x <+> text "*" <+> ppv y)
                MulValueHigh _ x y -> text "high" <> parens (ppv x <+> text "*" <+> ppv y)
                DivValue _ x y z -> ppv x <> char ':' <> ppv y <+> text "div" <+> ppv z
                ModValue _ x y z -> ppv x <> char ':' <> ppv y <+> text "mod" <+> ppv z
                NotValue x  -> text "~" <> ppv x
                AndValue x y -> ppv x <+> text "&" <+> ppv y
                OrValue x y  -> ppv x <+> text "|" <+> ppv y
                XorValue x y -> ppv x <+> text "xor" <+> ppv y
                ShlValue x y -> ppv x <+> text "<<" <+> ppv y
                ShrValue x y -> ppv x <+> text ">>" <+> ppv y

                LeastSignificantOne x -> text "least1" <> parens (ppv x)
                Trunc16To8  x -> text "trunc16_8"  <> parens (ppv x)
                Trunc32To8  x -> text "trunc32_8"  <> parens (ppv x)
                Trunc32To16 x -> text "trunc32_16" <> parens (ppv x)
                Trunc64To8  x -> text "trunc64_8"  <> parens (ppv x)
                Trunc64To16 x -> text "trunc64_16" <> parens (ppv x)
                Trunc64To32 x -> text "trunc64_32" <> parens (ppv x)

                ZExt1To8   x -> text "zext1_8"   <> parens (pp1 x)
                ZExt8To16  x -> text "zext8_16"  <> parens (ppv x)
                ZExt8To32  x -> text "zext8_32"  <> parens (ppv x)
                ZExt8To64  x -> text "zext8_64"  <> parens (ppv x)
                ZExt16To32 x -> text "zext16_32" <> parens (ppv x)
                ZExt16To64 x -> text "zext16_64" <> parens (ppv x)
                ZExt32To64 x -> text "zext32_64" <> parens (ppv x)

                SExt32To64 x -> text "sext32_64" <> parens (ppv x)

            ExtValue8  v -> ppV8  pp v
            ExtValue16 v -> ppV16 pp v
            ExtValue32 v -> ppV32 pp v
            ExtValue64 v -> ppV64 pp v



------------------------------------------------------------------------
-- InterruptNo

newtype InterruptNo = InterruptNo { interruptNoIdx :: Word8 }

de :: InterruptNo
de = InterruptNo 0

db :: InterruptNo
db = InterruptNo 1

------------------------------------------------------------------------
-- SemanticsMonad

class (Applicative m, Monad m) => SemanticsMonad m where
  mux :: Value1 m -> m () -> m () -> m ()

  getPC :: m (ValueOf m Word64)
  setPC :: ValueOf m Word64 -> m ()

  incPC :: SemanticsMonad m => Int64 -> m ()


  getMem :: TypeOf tp -> ValueOf m Word64 -> m (ValueOf m tp)
  setMem :: TypeOf tp -> ValueOf m Word64 -> ValueOf m tp -> m ()

  getReg8 :: Reg8 -> m (ValueOf m Word8)
  setReg8 :: Reg8 -> ValueOf m Word8 -> m ()

  getReg16 :: Reg16 -> m (ValueOf m Word16)
  setReg16 :: Reg16 -> ValueOf m Word16 -> m ()

  getReg32 :: Reg32 -> m (ValueOf m Word32)
  setReg32 :: Reg32 -> ValueOf m Word32 -> m ()

  getReg64 :: Reg64 -> m (ValueOf m Word64)
  setReg64 :: Reg64 -> ValueOf m Word64 -> m ()

  getFlag :: RFLAGS -> m (Value1 m)   
  setFlag :: RFLAGS -> Value1 m -> m ()

  raiseInterrupt :: InterruptNo -> m ()
  exec_syscall :: m ()



evalCond :: SemanticsMonad m => Cond -> m (Value1 m)
evalCond (And l)      = AndValue1 <$> traverse evalCond l
evalCond (Or  l)      = OrValue1  <$> traverse evalCond l
evalCond (EqCond x y) = EqValue1  <$> evalCond x <*> evalCond y
evalCond (Not x)      = NotValue1 <$> evalCond x
evalCond (RFLAGS_Set f) = getFlag f

whenCond :: SemanticsMonad m => Cond -> m () -> m ()
whenCond c t = do
  v <- evalCond c
  mux v t (return ())

resolveAddr :: SemanticsMonad m => AddrRef -> m (ValueOf m Word64)
resolveAddr (IP_Offset_64 _ o) = do
  mpc <- getPC
  return $ mpc .+ cnsValue Type64 (toInteger o)
resolveAddr (Addr_64 _ (Just r) mi o) = do
  rv <- getReg64 r
  let mkCns :: Integral a => a -> ValueOf m Word64
      mkCns = cnsValue Type64 . fromIntegral
  rv' <- case mi of
           Nothing -> return rv
           Just (s,i) -> (\iv -> rv .+ (mkCns s `mul_low` iv)) <$> getReg64 i
  return $ rv' .+ mkCns o
resolveAddr a = fail $ "Unsupported address " ++ show a

resolveValueAddr :: SemanticsMonad m => TypeOf tp -> Value -> m (ValueOf m Word64)
resolveValueAddr Type8  (Mem8  a) = resolveAddr a
resolveValueAddr Type16 (Mem16 a) = resolveAddr a
resolveValueAddr Type32 (Mem32 a) = resolveAddr a
resolveValueAddr Type64 (Mem64 a) = resolveAddr a
resolveValueAddr _    (VoidMem a) = resolveAddr a

resolveValueAddr tp v = fail $ "resolveValueAddr given value that is not an address: " ++ show (tp, v)

resolveValue :: SemanticsMonad m => TypeOf tp -> Value -> m (ValueOf m tp)
resolveValue Type8  (ByteReg r)  = getReg8 r
resolveValue Type16 (WordReg r)  = getReg16 r
resolveValue Type32 (DWordReg r) = getReg32 r
resolveValue Type64 (QWordReg r) = getReg64 r

resolveValue Type8  (Mem8  a)    = getMem Type8  =<< resolveAddr a
resolveValue Type16 (Mem16 a)    = getMem Type16 =<< resolveAddr a
resolveValue Type32 (Mem32 a)    = getMem Type32 =<< resolveAddr a
resolveValue Type64 (Mem64 a)    = getMem Type64 =<< resolveAddr a

resolveValue Type8  (ByteImm c)  = return $ cnsValue Type8 (toInteger c)
resolveValue Type32 (DWordImm c) = return $ cnsValue Type32 (toInteger c)
resolveValue Type64 (QWordImm v) = return $ cnsValue Type64 (toInteger v)

resolveValue tp v = error $ "resolveValue undefined with " ++ show (tp, v)

switchType :: Value -> (forall tp . TypeOf tp -> a) -> a
switchType (ByteReg  _) f = f Type8
switchType (WordReg _)  f = f Type16
switchType (DWordReg _) f = f Type32
switchType (QWordReg _) f = f Type64 
switchType (Mem8  _)    f = f Type8
switchType (Mem32 _)    f = f Type32
switchType (Mem64 _)    f = f Type64
switchType v _ = error $ "switchType: unsupported value " ++ show v

setValue :: SemanticsMonad m => Value -> ValueOf m tp -> m ()
setValue (ByteReg  r) v | Type8  <- typeOfOperand v = setReg8  r v
setValue (WordReg r) v  | Type16 <- typeOfOperand v = setReg16 r v
setValue (DWordReg r) v | Type32 <- typeOfOperand v = setReg32 r v
setValue (QWordReg r) v | Type64 <- typeOfOperand v = setReg64 r v
setValue (Mem8  a) v | Type8 <- typeOfOperand v = do
  lhs <- resolveAddr a
  setMem Type8 lhs v
setValue (Mem32 a) v | Type32 <- typeOfOperand v = do
  lhs <- resolveAddr a
  setMem Type32 lhs v
setValue (Mem64 a) v | Type64 <- typeOfOperand v = do
  lhs <- resolveAddr a
  setMem Type64 lhs v
setValue lhs _ = fail $ "setValue given " ++ show lhs

-- | Update flags with given result value.
set_result_flags :: SemanticsMonad m => ValueOf m tp -> m ()
set_result_flags r = do
  sf `setFlag` msb r
  zf `setFlag` is_zero r
  pf `setFlag` even_parity (least_byte r)

-- | Update flags in accordance with a subtraction of the second value from the first.
set_sub_flags :: SemanticsMonad m => ValueOf m tp -> ValueOf m tp -> m ()
set_sub_flags xv yv = do
  of_rflag `setFlag` SSub_Overflows xv yv
  cf       `setFlag` USub_Overflows xv yv
  af       `setFlag` USub4_Overflows (least_byte xv) (least_byte yv)
  set_result_flags $ xv .- yv

-- | Assign value to location and update corresponding flags.
set_result_value :: SemanticsMonad m => Value -> ValueOf m tp -> m ()
set_result_value x r = set_result_flags r >> setValue x r

exec_cmov :: SemanticsMonad m => Cond => Value -> Value -> m ()
exec_cmov c lhs m = switchType lhs $ \tp -> do
  let falseCase =
        case tp of
          Type64 -> setValue lhs . zeroTop32 =<< resolveValue tp lhs
          _ -> return ()
  v <- evalCond c
  mux v (setValue lhs =<< resolveValue tp m) falseCase

-- | Set flags in list to not @UndefValue1@.
set_undef :: SemanticsMonad m => [RFLAGS] -> m ()
set_undef = traverse_ (`setFlag` UndefValue1)

exec_div :: SemanticsMonad m
         => TypeOf tp
         -> (r -> m (ValueOf m tp))
         -> (r -> ValueOf m tp -> m ())
         -> r
         -> r
         -> Value
         -> m ()
exec_div tp getReg setReg hr lr x = do
  src <- resolveValue tp x       
  h <- getReg hr
  l <- getReg lr
  mux (Div_Overflows h l src)
      (raiseInterrupt de)
      (do setReg lr $ VF $ DivValue tp h l src
          setReg hr $ VF $ ModValue tp h l src
          set_undef [ zf, cf, of_rflag, sf, af, pf ])

-- | Return offset to use for incrementing rsi or rdi in a string
-- instruction.
str_offset :: SemanticsMonad m => Word64 -> m (ValueOf m Word64)
str_offset o = do
  dfv <- getFlag df
  return $ VF $ IteValue dfv (cnsValue Type64 (toInteger (negate o)))
                             (cnsValue Type64 (toInteger o))

-- | Return bit-pattern to mask shift value with
shift_mask :: TypeOf tp -> ValueOf m Word8
shift_mask Type64 = cnsValue Type8 0x3f
shift_mask _      = cnsValue Type8 0x1f

exec_mul :: SemanticsMonad m
         => TypeOf tp
         -> (r -> m (ValueOf m tp))
         -> (r -> ValueOf m tp -> m ())
         -> r -- ^ Register with first operand and destination to store low width(tp) bytes. 
         -> r -- ^ Register to store high width(tp) bytes. 
         -> Value -- ^ Second operand
         -> m ()
exec_mul tp getReg setReg lr hr src = do
  xv <- getReg lr
  yv <- resolveValue tp src
  let high_result = xv `mul_high` yv
  setReg lr high_result
  setReg hr (xv `mul_low` yv)
  let c = NotValue1 (IsZero high_result)
  of_rflag `setFlag` c
  cf       `setFlag` c
  set_undef [sf, zf, af, pf]

set_bitwise_flags :: SemanticsMonad m => ValueOf m tp -> m ()
set_bitwise_flags r = do
  of_rflag `setFlag` CnsValue1 False
  cf       `setFlag` CnsValue1 False
  set_undef [af]
  set_result_flags r

is_above :: Cond 
is_above = And [is_unset cf, is_unset zf]

is_below :: Cond 
is_below = is_set cf

is_notzero :: Cond
is_notzero = is_unset zf

exec_setcc :: SemanticsMonad m => Cond -> Value -> m ()
exec_setcc c dst = do
  v <- evalCond c
  setValue dst $ VF $ ZExt1To8 v

exec :: SemanticsMonad m => Addr -> String -> [Value] -> m ()
exec _ "add" [x, y] = switchType x $ \tp -> do
  xv <- resolveValue tp x
  yv <- resolveValue tp y
  of_rflag `setFlag` sadd_overflows xv yv
  af       `setFlag` uadd4_overflows (least_byte xv) (least_byte yv)
  cf       `setFlag` uadd_overflows xv yv
  set_result_value x $ xv .+ yv
exec _ "and" [x,y] = 
  switchType x $ \tp -> do
    rv <- (.&.) <$> resolveValue tp x
                <*> resolveValue tp y
    set_bitwise_flags rv
    x `setValue` rv
exec _ "bsf" [x,y] = switchType x $ \tp -> do
  yv <- resolveValue tp y
  zf       `setFlag` is_zero yv
  set_undef [ cf, of_rflag, sf, af, pf ]
exec _ "call" [x] = do
  pc <- getPC
  -- Update pc.
  case x of
    JumpOffset o -> incPC o
    QWordReg r -> setPC =<< getReg64 r
    _ -> fail $ "Unsupported call type " ++ show x
  -- Get top of stack
  stackLoc <- getReg64 rsp 
  -- Push stack location by 8 bytes.
  setMem Type64 stackLoc pc
  setReg64 rsp (stackLoc .- cnsValue Type64 8)
exec _ "cld" [] = do
  df `setFlag` CnsValue1 False
exec _ "cmovbe" [l,r] = exec_cmov (Or [is_set cf, is_set zf]) l r
exec _ "cmovz"  [l,r] = exec_cmov (is_set zf) l r
exec _ "cmp" [x,y] = switchType x $ \tp -> do
  xv <- resolveValue tp x
  yv <- resolveValue tp y
  set_sub_flags xv yv
exec _ "div" [x] =
  switchType x $ \tp ->
    case tp of
      Type8  -> exec_div tp getReg8  setReg8   ah  al x
      Type16 -> exec_div tp getReg16 setReg16  dx  ax x
      Type32 -> exec_div tp getReg32 setReg32 edx eax x
      Type64 -> exec_div tp getReg64 setReg64 rdx rax x
exec _ "imul" [x,y] = switchType x $ \tp -> do
  xv <- resolveValue tp x
  yv <- resolveValue tp y
  let overflow = STimes_Overflows xv yv
  cf       `setFlag` overflow
  of_rflag `setFlag` overflow
  set_undef [ zf, sf, af, pf ]
  x `setValue` (xv `mul_low` yv)
-- Jump short if above
exec _ "ja"  [JumpOffset o] = whenCond is_above $ incPC o
exec _ "jae" [JumpOffset o] = whenCond (is_unset cf) $ incPC o
exec _ "jb"  [JumpOffset o] = whenCond is_below $ incPC o
exec _ "jbe" [JumpOffset o] = do
  whenCond (Or [is_set cf, is_set zf]) $ do
    incPC o
exec _ "jge" [JumpOffset o] = do
  whenCond (sf .== of_rflag) $ do
    incPC o
exec _ "jle" [JumpOffset o] = do
  whenCond (Or [is_set zf, sf ./= of_rflag]) $ do
    incPC o
exec _ "jmp" [JumpOffset o] = do
  incPC o
exec _ "jmp" [QWordReg r] = do
  setPC =<< getReg64 r
exec _ "jns" [JumpOffset o] = do
  whenCond (is_unset sf) $ do
    incPC o
exec _ "jnz" [JumpOffset o] = do
  whenCond is_notzero $ incPC o
exec _ "js" [JumpOffset o] = do
  whenCond (is_set sf) $ do
    incPC o
exec _ "jz" [JumpOffset o] = do
  whenCond (is_set zf) $ do
    incPC o
exec _ "lea" [x,y] = switchType x $ \tp -> do
  yv <- resolveValueAddr tp y
  x `setValue` trunc64To yv tp
exec _ "mov" [lhs, m] = do
  switchType lhs $ \tp -> do
    setValue lhs =<< resolveValue tp m
exec _ "movsb" [] = do
  -- Compute which way to adjust points.
  o <- str_offset 1
  -- Read source and increment
  src_addr <- getReg64 rsi
  setReg64 rsi (src_addr .+ o)
  -- Read dest and increment
  dst_addr <- getReg64 rdi
  setReg64 rdi (dst_addr .+ o)
  -- Copy byte from src address to dest.
  setMem Type8 dst_addr =<< getMem Type8 src_addr
exec _ "movsxd" [dst,src] = do
  srcv <- resolveValue Type32 src
  setValue dst (VF $ SExt32To64 srcv)
exec _ "movzx" [dst, src] = do
  switchType src $ \src_tp -> do
    srcv <- resolveValue src_tp src
    switchType dst $ \dst_tp -> do
      setValue dst (zext src_tp srcv dst_tp)
exec _ "mul" [src] = do
  switchType src $ \tp -> do
    case tp of
      Type8  -> exec_mul Type8  getReg8  setReg8  al  ah src
      Type16 -> exec_mul Type16 getReg16 setReg16 ax  dx src
      Type32 -> exec_mul Type32 getReg32 setReg32 eax edx src
      Type64 -> exec_mul Type64 getReg64 setReg64 rax rdx src
exec _ "neg" [x] = do
  switchType x $ \tp -> do
     v <- resolveValue tp x
     cf       `setFlag` NotValue1 (IsZero v)
     af       `setFlag` USub4_Overflows (cnsValue Type8 0) (least_byte v)
     set_result_value x (negV v)
exec _ "nop" [_] = do
  return ()
exec _ "not" [x] = do
  switchType x $ \tp -> do
    v <- resolveValue tp x
    x `setValue` complement v
exec _ "or" [x,y] = do
  switchType x $ \tp -> do
    rv <- (.|.) <$> resolveValue tp x
                <*> resolveValue tp y
    set_bitwise_flags rv
    x `setValue` rv
exec _ "pop" [x] = do
  stackLoc <- getReg64 rsp
  switchType x $ \tp -> do
    -- Set stack loc value
    v <- getMem tp stackLoc
    x `setValue` v
    -- Increment stack pointer by @bytew tp@ bytes.
    setReg64 rsp $ stackLoc .+ cnsValue Type64 (bytew tp)
exec _ "push" [x] = do
  stackLoc <- getReg64 rsp
  switchType x $ \tp -> do
    -- Push value to stack location.
    setMem tp stackLoc =<< resolveValue tp x
    -- Decrement stack pointer by @bytew tp@ bytes.
    setReg64 rsp $ stackLoc .+ cnsValue Type64 (bytew tp)
exec _ "ret" [] = do
  -- Get top of stack
  stackLoc <- getReg64 rsp
  -- Pop top 8-bytes off stack.
  setReg64 rsp (stackLoc .+ cnsValue Type64 8)
  -- Set pc from top of stack
  setPC =<< getMem Type64 stackLoc
exec _ "seta" [dst] = do
  exec_setcc is_above dst
exec _ "setb" [dst] = do
  exec_setcc is_below dst
exec _ "setnz" [dst] = do
  exec_setcc is_notzero dst
exec _ "shl" [x,y] = do
  switchType x $ \tp -> do
    xv <- resolveValue tp x
    sv <- (shift_mask tp .&.) <$> resolveValue Type8 y
    let isZ = IsZero sv
        -- Set a flag if y is non-zero.
    let setFlagIfNz f new_value = do
          p <- getFlag f
          f `setFlag` MuxValue1 isZ p new_value
    let carry_value = xv `bit_at` (cnsValue Type8 (bitw tp) .- sv)
    cf       `setFlagIfNz` carry_value 
    -- New value for overflow flag to be used only if shift value is 1.
    let of1_value =   bit_at xv (cnsValue Type8 (bitw tp - 1))
         `xor_value1` bit_at xv (cnsValue Type8 (bitw tp - 2))
    -- Set of flag to of1_value if shift value is 1, otherwise set it to undef
    -- if flag is non-zero.
    of_rflag `setFlagIfNz` MuxValue1 (IsEq sv (cnsValue Type8 1)) of1_value UndefValue1
    af `setFlagIfNz` UndefValue1

    let r = VF $ ShlValue xv sv
    sf `setFlagIfNz` msb r
    zf `setFlagIfNz` is_zero r
    pf `setFlagIfNz` even_parity (least_byte r)
    setValue x r
exec _ "shr" [x,y] = do
  switchType x $ \tp -> do
    xv <- resolveValue tp x
    yv <- resolveValue Type8 y
    let sv = shift_mask tp .&. yv
    let isZ = IsZero sv
        -- Set a flag if y is non-zero.
    let setFlagIfNz f new_value = do
          p <- getFlag f
          f `setFlag` MuxValue1 isZ p new_value
    let carry_value = xv `bit_at` (sv .- cnsValue Type8 1)
    cf       `setFlagIfNz` carry_value
    -- New value for overflow flag to be used only if shift value is 1.
    let of1_value = msb xv 
    -- Set of flag to of1_value if shift value is 1, otherwise set it to undef
    -- if flag is non-zero.
    of_rflag `setFlagIfNz` MuxValue1 (IsEq sv (cnsValue Type8 1)) of1_value UndefValue1
    af       `setFlagIfNz` UndefValue1
    let r = VF $ ShrValue xv sv
    sf `setFlagIfNz` msb r
    zf `setFlagIfNz` is_zero r
    pf `setFlagIfNz` even_parity (least_byte r)
    setValue x r
exec _ "stosb" [] = do
  o <- str_offset 1
  -- Read dest and increment
  dst_addr <- getReg64 rdi
  setReg64 rdi (dst_addr .+ o)
  -- Copy byte from al to dest.
  setMem Type8 dst_addr =<< getReg8 al
exec _ "stosq" [] = do
  o <- str_offset 1
  -- Read dest and increment
  dst_addr <- getReg64 rdi
  setReg64 rdi (dst_addr .+ o)
  -- Copy byte from al to dest.
  setMem Type64 dst_addr =<< getReg64 rax
exec _ "sub" [x,y] =
  switchType x $ \tp -> do
    xv <- resolveValue tp x
    yv <- resolveValue tp y
    set_sub_flags xv yv
    setValue x (xv .- yv)
exec _ "syscall" [] =
  exec_syscall
exec _ "test" [x, y] = do
  switchType x $ \tp -> do
    rv <- (.&.) <$> resolveValue tp x
                <*> resolveValue tp y
    set_bitwise_flags rv
exec _ "xchg" [x,y] = do
  switchType x $ \tp -> do
    xv <- resolveValue tp x
    yv <- resolveValue tp y
    x `setValue` yv
    y `setValue` xv
exec _ "xor" [x, y] = do
  switchType x $ \tp -> do
    xv <- resolveValue tp x
    yv <- resolveValue tp y
    of_rflag `setFlag` CnsValue1 False
    cf       `setFlag` CnsValue1 False
    set_undef [af]
    set_result_value x (xv `xor` yv)
exec w nm l = fail $ "Unsupported instruction at " ++ show w ++ ":\n" ++ nm ++ " " ++ show l