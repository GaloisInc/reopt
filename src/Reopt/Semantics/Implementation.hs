{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Reopt.Semantics.Implementation
  ( cfgFromAddress
  , completeProgram
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.ST
import Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.TypeLits
import Data.Parameterized.NatRepr

import Reopt.Memory
import Reopt.Semantics.Monad

------------------------------------------------------------------------
-- Location

{-
   = CF_FLAG
   | DF_FLAG
   | PF_FLAG
   | AF_FLAG
   | ZF_FLAG
   | SF_FLAG
   | OF_FLAG
-}

{-
instance Enum FlagReg where
  toEnum 0 = CF_FLAG
  toEnum 1 = PF_FLAG
  toEnum 2 = AF_FLAG
  toEnum 3 = ZF_FLAG
  toEnum 4 = SF_FLAG
  toEnum 5 = OF_FLAG
  toEnum _ = error "Unexpected numerator."

  fromEnum CF_FLAG = 0
  fromEnum PF_FLAG = 1
  fromEnum AF_FLAG = 2
  fromEnum ZF_FLAG = 3
  fromEnum SF_FLAG = 4
  fromEnum OF_FLAG = 5
-}

{-
  af_flag = FlagReg AF_FLAG
  cf_flag = FlagReg CF_FLAG
  df_flag = FlagReg DF_FLAG
  of_flag = FlagReg OF_FLAG
  pf_flag = FlagReg PF_FLAG
  sf_flag = FlagReg SF_FLAG
  zf_flag = FlagReg ZF_FLAG
-}

------------------------------------------------------------------------
-- Expr IsValue instance

{-
data Expr s tp where
   FalseExpr :: Expr s BoolType
   TrueExpr :: Expr s BoolType
-}

{-
instance IsValue (Expr s) where
  false = C.false
  true  = C.true
-}

------------------------------------------------------------------------
-- X86ProgramCFG

type Addr = Word64

data Block = Block { blockLabel :: Addr
                   , blockStmts :: [Stmt]
                   }

data Stmt where
  UndefinedStmt :: Stmt

-- | A complete CFG is a map from all reachable code locations
-- to the block for that code location.
type X86ProgramCFG = Map Addr Block

------------------------------------------------------------------------
-- X86State

{-
data X64State s
   = X64State { regs  :: V.Vector (C.Reg s (BVType 64))
              , flags :: V.Vector (C.Reg s C.BoolType)
              }
-}

data App f tp where
  -- Concatenate two bitvectors together (low-bits are first)
  ConcatV :: {-#UNPACK #-} !(NatRepr n)
          -> !(f (BVType n))
          -> !(f (BVType n))
          -> App f (BVType (n+n))
  AndApp :: !(f BoolType) -> !(f BoolType) -> App f BoolType
  OrApp  :: !(f BoolType) -> !(f BoolType) -> App f BoolType
  NotApp :: !(f BoolType) -> App f BoolType

data Expr tp where
  Expr :: !(TypeRepr tp) -> !(App Expr tp) -> Expr tp
  TrueExpr :: Expr BoolType
  FalseExpr :: Expr BoolType

instance IsValue Expr where
  true  = TrueExpr
  false = FalseExpr

data X86State = X86State
     { _flagRegs :: Expr (BVType 64)
     }

flagRegs :: Simple Lens X86State (Expr (BVType 64))
flagRegs = lens _flagRegs (\s v -> s { _flagRegs = v })

------------------------------------------------------------------------
-- X86Generator

newtype X86Generator a = X86G { unX86G :: State X86State a }
  deriving (Functor, Applicative, Monad)

type instance Value X86Generator = Expr

--getPosition :: X86Generator s C.Position
--getPosition = undefined

--getFlagReg :: FlagReg -> X86Generator s (C.Reg s BoolType)
--getFlagReg = undefined

{-
getFlagReg f = do
  fr <- gets flagRegs
  let Just r = fr V.!? fromEnum f
  return r
-}

instance Semantics X86Generator where

{-  set_undefined (FlagReg f) = do
    p <- getPosition
    r <- getFlagReg f
    C.assignReg p r C.false

  get (FlagReg f) = do
    p <- getPosition
    r <- getFlagReg f
    E <$> C.readReg p r

  FlagReg f .= (E e) = do
    p <- getPosition
    r <- getFlagReg f
    C.assignReg p r e
-}


cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> Word64
                  -- ^ Address to start disassembler form.
               -> X86ProgramCFG
cfgFromAddress = error "Reopt.Semantics.Implementation.cfgFromAddress undefined"


completeProgram :: Memory Word64
                -> Addr
                -> X86ProgramCFG
completeProgram mem addr = do
  error "completeProgram undefined"

{-
resolve :: Memory Word64
        -> [Word64]
        -> X86ProgramCFG s
        -> ST s (X86ProgramCFG s)
resolve mem [] pg = return pg
resolve mem (a:r) pg = do
  g <- cfgFromAddress mem a
  resolve mem r (Map.insert a g)
-}