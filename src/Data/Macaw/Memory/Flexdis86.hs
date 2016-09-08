{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Macaw.Memory.Flexdis86
  ( MemoryByteReader
  , runMemoryByteReader
  , readInstruction
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Macaw.Memory
import           Data.Word

import qualified Flexdis86 as Flexdis
import           Flexdis86.ByteReader

------------------------------------------------------------------------
-- MemStream

data MemStream w = MS { msNext :: !BS.ByteString
                      , msMem  :: !(Memory w)
                      , msAddr :: !w
                      , msPerm :: !ElfSegmentFlags
                        -- ^ Permissions that memory accesses are expected to satisfy.
                      }

------------------------------------------------------------------------
-- MemoryByteReader

newtype MemoryByteReader w a = MBR { unMBR :: ExceptT (MemoryError w) (State (MemStream w)) a }
  deriving (Functor, Applicative, MonadError (MemoryError w))

instance Monad (MemoryByteReader w) where
  return  = MBR . return
  MBR m >>= f = MBR $ m >>= unMBR . f
  fail    = throwError . UserMemoryError

-- | Create a memory stream pointing to given address, and return pair whose
-- first element is the value read or an error, and whose second element is
-- the address of the next value to read.
runMemoryByteReader :: ElfSegmentFlags
                       -- ^ Permissions that memory accesses are expected to
                       -- satisfy.
                       -- Added so we can check for read and/or execute permission.
                    -> Memory w -- ^ Memory to read from.
                    -> w -- ^ Starting address.
                    -> MemoryByteReader w a -- ^ Byte reader to read values from.
                    -> (Either (MemoryError w) (a, w))
runMemoryByteReader reqPerm mem addr (MBR m) =
  case runState (runExceptT m) (MS BS.empty mem addr reqPerm) of
    (Left e, _) -> Left e
    (Right v, s) -> Right (v,msAddr s)

instance (Integral w, Show w) => ByteReader (MemoryByteReader w) where
  readByte = do
    MS b m w reqPerm <- MBR get
    if BS.null b then
      case findSegment w m of
        Nothing -> MBR $ throwError $ AccessViolation w
        Just s -> assert (memBase s <= w) $ do
          MBR $ do
            -- Throw error when permissions check fails.
            when ((memFlags s .&. reqPerm) /= reqPerm) $ do
              throwError $ PermissionsError w
            -- Let d be number of bytes to drop from start of segment.
            let d = fromIntegral (w - memBase s)
            put MS { msNext = BS.drop d (memBytes s)
                   , msMem = m
                   , msAddr = w
                   , msPerm = reqPerm
                   }
          readByte
     else do
      let v = BS.head b
      let ms = MS { msNext = BS.tail b
                  , msMem = m
                  , msAddr = w+1
                  , msPerm = reqPerm
                  }
      MBR $ v <$ put ms

------------------------------------------------------------------------
-- readInstruction

-- | Read instruction at a given memory address.
readInstruction :: Memory Word64 -- Memory to read.
                -> Word64 -- Address to read from.
                -> Either (MemoryError Word64) (Flexdis.InstructionInstance, Word64)
readInstruction mem addr = runMemoryByteReader pf_x mem addr m
  where m = Flexdis.disassembleInstruction
