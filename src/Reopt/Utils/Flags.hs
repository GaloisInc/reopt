{-| Primitive function related to testing flags. -}
module Reopt.Utils.Flags ( hasFlags ) where

import Data.Bits

-- | `hasFlag x a` returns true if `x` has all the flags in `a`.
hasFlags :: Bits a => a -> a -> Bool
hasFlags x a = x .&. a == a