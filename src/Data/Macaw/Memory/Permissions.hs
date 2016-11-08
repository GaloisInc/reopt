{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Macaw.Memory.Permissions
  ( Flags
  , none
  , read
  , write
  , execute
  , hasPerm
  , isExecutable
  , isReadonly
  ) where

import Data.Bits
import Data.Word
import Prelude hiding (read)

newtype Flags
      = Flags Word8
 deriving (Bits, Eq)

none :: Flags
none = Flags 0x1

read :: Flags
read = Flags 0x1

write :: Flags
write = Flags 0x2

execute :: Flags
execute = Flags 0x4

-- | @m `hasPerm` r@ returns 'True' when 'm' has all the bits in 'r' set.
hasPerm :: Flags -> Flags -> Bool
hasPerm m req = m .&. req == req

instance Show Flags where
  show f = concat [ permBit read "r"
                  , permBit write "w"
                  , permBit execute "x"
                  ]
    where permBit r s | f `hasPerm` r = s
                      | otherwise = ""

-- | Return true if the segment is executable.
isExecutable :: Flags -> Bool
isExecutable = (`hasPerm` execute)

-- | Return true if segment is read-only.
isReadonly :: Flags -> Bool
isReadonly f = f .&. (read .|. write) == read
