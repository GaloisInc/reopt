module Reopt.CFG.JumpTargets where

import Control.Monad.State

data S = S ()

newtype M a = M { unM :: State S a }