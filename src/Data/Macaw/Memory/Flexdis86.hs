{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Macaw.Memory.Flexdis86
  ( MemoryByteReader
  , runMemoryByteReader
  , readInstruction
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS

import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm

import qualified Flexdis86 as Flexdis
import           Flexdis86.ByteReader

------------------------------------------------------------------------
-- MemStream

data MemStream w = MS { msNext :: ![SegmentRange w]
                      , msMem  :: !(Memory w)
                      , msAddr :: !(SegmentedAddr w)
                      , msPerm :: !Perm.Flags
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
runMemoryByteReader :: Integral (MemWord w)
                    => Perm.Flags
                       -- ^ Permissions that memory accesses are expected to
                       -- satisfy.
                       -- Added so we can check for read and/or execute permission.
                    -> Memory w -- ^ Memory to read from.
                    -> SegmentedAddr w -- ^ Starting address.
                    -> MemoryByteReader w a -- ^ Byte reader to read values from.
                    -> Either (MemoryError w) (a, SegmentedAddr w)
runMemoryByteReader reqPerm mem addr (MBR m) = do
  let seg = addrSegment addr
  if not (segmentFlags seg `Perm.hasPerm` reqPerm) then
    Left $ PermissionsError addr
   else do
    contents <- addrContentsAfter addr
    let ms0 = MS { msNext = contents
                 , msMem  = mem
                 , msAddr = addr
                 , msPerm = reqPerm
                 }
    case runState (runExceptT m) ms0 of
      (Left e, _) -> Left e
      (Right v, s) -> Right (v,msAddr s)

instance Num (MemWord w) => ByteReader (MemoryByteReader w) where
  readByte = do
    ms <- MBR get
    -- If remaining bytes are empty
    case msNext ms of
      [] -> MBR $ throwError $ AccessViolation (msAddr ms)
      RelocatableAddr{}:_ -> do
        MBR $ throwError $ UnalignedRelocation (msAddr ms)
      ByteRegion bs:rest -> do
        if BS.null bs then do
          MBR $ throwError $ AccessViolation (msAddr ms)
         else do
          let v = BS.head bs
          let ms' = ms { msNext = ByteRegion (BS.tail bs) : rest
                       , msAddr = msAddr ms & addrOffset +~ 1
                       }
          MBR $ v <$ put ms'

------------------------------------------------------------------------
-- readInstruction

-- | Read instruction at a given memory address.
readInstruction :: Memory 64 -- Memory to read.
                -> SegmentedAddr 64 -- Address to read from.
                -> Either (MemoryError 64)
                          (Flexdis.InstructionInstance, SegmentedAddr 64)
readInstruction mem addr = runMemoryByteReader Perm.execute mem addr m
  where m = Flexdis.disassembleInstruction
