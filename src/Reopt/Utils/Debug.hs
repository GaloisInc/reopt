
module Reopt.Utils.Debug
       ( DebugClass(..)
       , debugKeys
       , debug
       , debug'         
       , debugM
       , debugM'
       ) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Debug.Trace

debugKeys :: [DebugClass]
debugKeys = [DUrgent, DFunRecover]

-- Basically a tag we can use to turn on/off debug messages (only at
-- compile time though).
data DebugClass = DUrgent | DAbsInt | DCFG | DFunRecover
                deriving (Eq, Ord, Show)

debug :: DebugClass -> String -> a -> a
debug cl msg x
  | cl`elem` debugKeys = trace msg x
  | otherwise = x

debug' :: DebugClass -> Doc -> a -> a
debug' cl msg x = debug cl (show msg) x

debugM :: Monad m => DebugClass -> String -> m () 
debugM cl msg 
  | cl `elem` debugKeys = traceM msg
  | otherwise = return ()

debugM' :: Monad m => DebugClass -> Doc -> m () 
debugM' cl msg = debugM cl (show msg)
