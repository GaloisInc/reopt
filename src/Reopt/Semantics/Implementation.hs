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
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Class
import Control.Monad.ST
import Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word

import qualified Lang.Crucible.Generator as C
import qualified Lang.Crucible.Syntax as C
import Lang.Crucible.Types
import Lang.Crucible.Utils.TypeContext

import Reopt.Memory
import Reopt.Semantics.Monad

------------------------------------------------------------------------
-- Location

-- | A flag register.
data FlagReg
   = CF_FLAG
   | PF_FLAG
   | AF_FLAG
   | ZF_FLAG
   | SF_FLAG
   | OF_FLAG

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

data Location s tp where
  FlagReg :: !(FlagReg) -> Location s Bool

instance IsAssignable (Location s) where
  af_flag = FlagReg AF_FLAG
  cf_flag = FlagReg CF_FLAG
  of_flag = FlagReg OF_FLAG
  pf_flag = FlagReg PF_FLAG
  sf_flag = FlagReg SF_FLAG
  zf_flag = FlagReg ZF_FLAG

------------------------------------------------------------------------
-- Expr IsValue instance

newtype Expr s tp = E (C.Expr s tp)
  deriving (C.IsExpr)

{-
instance IsValue (Expr s) where
  false = C.false
  true  = C.true
-}

------------------------------------------------------------------------
-- X86State

-- | Components of an x86 processor state.
type X86Fields = EmptyCtx

type Word64Type = BVType (UnsignedBV 64) StandardStyle


data X64State s
   = X64State { regs  :: V.Vector (C.Reg s Word64Type)
              , flags :: V.Vector (C.Reg s BoolType)
              }

{-
data X86State s = X86State
     { flagRegs :: Vector (C.Reg s Bool)
     }
-}

------------------------------------------------------------------------
-- X86Generator

type X86Generator s = C.Generator s X64State UnitType

type instance Assignable (X86Generator s) = Location s
--type instance Value (X86Generator s) = Expr s

getPosition :: X86Generator s C.Position
getPosition = undefined

getFlagReg :: FlagReg -> X86Generator s (C.Reg s BoolType)
getFlagReg = undefined
{-
getFlagReg f = do
  fr <- gets flagRegs
  let Just r = fr V.!? fromEnum f
  return r
-}

{-
instance Semantics (X86Generator s) where
  set_undefined (FlagReg f) = do
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

type StructType args = Assignment Identity args

type X86State = BoolType -- TODO: Figure this out.

type X86Inputs = EmptyCtx ::> BoolType
-- TODO: Populate this.

-- | Control flow graph for x86 basic block.  This takes the processor
-- state as an argument, and returns the processor state as the output.
type X86BlockCFG s = C.CFG s X86Inputs UnitType


cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> Word64
                  -- ^ Address to start disassembler form.
               -> ST s (X86BlockCFG s)
cfgFromAddress = undefined


-- | A complete CFG is a map from all reachable code locations
-- to the block for that code location.
type X86ProgramCFG s = Map Word64 (X86BlockCFG s)


completeProgram :: Memory Word64
                -> Word64
                -> ST s (X86ProgramCFG s)
completeProgram mem addr = do
  undefined

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