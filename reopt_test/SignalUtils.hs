{- |
Module      :  $Header$
Description :  Utilities for POSIX signals.
Copyright   :  (c) Galois, Inc 2015
Maintainer  :  conathan@galois.com

Convert signals and @waitpid@ statuses to strings.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
module SignalUtils
  ( signalToString
  , statusToString
  ) where

import           Foreign.C (CInt(..), CString, peekCString)
import           System.Posix.Signals (Signal)
import           System.Posix.Waitpid (Status(..))

-- | "strsignal - return string describing signal".
foreign import ccall "string.h strsignal" strsignal :: CInt -> IO CString

-- | Convert a signal number to its Unix string representation.
--
-- A mapping from signal names to numbers is in @signal.h@, e.g.
-- @
-- /usr/src/linux-headers-3.16.0-44/arch/ia64/include/uapi/asm/signal.h
-- @ on conathan's machine.
signalToString :: Signal -> IO String
signalToString signal = do
  cString <- strsignal signal
  peekCString cString

-- | Convert a status to a string using 'signalToString' to convert embedded signals.
statusToString :: Status -> IO String
statusToString = \case
  Signaled signal -> go "Signaled" signal
  Stopped signal -> go "Stopped" signal
  status -> return $ show status
  where
    go prefix signal = do
      s <- signalToString signal
      let i = show signal
      return $ prefix ++ " " ++ i ++ " (" ++ s ++ ")"
