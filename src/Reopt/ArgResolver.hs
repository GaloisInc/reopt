{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reopt.ArgResolver
  ( ArgResolver
  , runArgResolver
  , ArgResolverError(..)
  , showArgResolverError
  , addGPReg64
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Flexdis86 as F
import           Text.Printf (printf)

import           Reopt.CFG.Recovery (X86ArgInfo(..))

-- | An error from parsing the external header for arguments
data ArgResolverError
   = OutOfGPRegs !String
     -- ^ @OutOfGPRegs nm@ The argument @nm@ has run out of general-purpose registers.
   | MissingArgType !String
     -- ^ Return type for argument is missing
   | UnsupportedArgType !String !String
     -- ^ @UnsupportedArgType nm tp@ Argument @nm@ does not support type.
   | UnsupportedReturnType !String
   | DebugResolveError !String
   | VarArgsUnsupported

-- | Pretty print for header errors.
showArgResolverError :: ArgResolverError -> String
showArgResolverError (OutOfGPRegs _) =
  "Stack arguments are not supported and no more general-purpose registers available."
showArgResolverError (MissingArgType vnm) =
  printf "Argument %s missing type." vnm
showArgResolverError (UnsupportedArgType vnm tp) =
  printf "Argument %s does not support type %s." vnm tp
showArgResolverError (UnsupportedReturnType tp) =
  printf "Do not support return argument type %s." tp
showArgResolverError (DebugResolveError msg) =
  msg
showArgResolverError VarArgsUnsupported =
  "Do not support vararg functions."

-- |  State monad for resolving arguments.
data ArgResolverState = ARS { arsPrev :: [X86ArgInfo]
                              -- ^ Arguments identified in reverse order.
                            , arsNextGPP :: [F.Reg64]
                              -- ^ General purpose registers still
                              -- available for arguments.
                            }

newtype ArgResolver a =
    ArgResolver (StateT ArgResolverState (ExceptT ArgResolverError IO) a)
  deriving (Functor, Applicative, Monad, MonadError ArgResolverError, MonadIO)

runArgResolver :: ArgResolver () -> ExceptT ArgResolverError IO [X86ArgInfo]
runArgResolver (ArgResolver m) =
  let s0 = ARS { arsPrev = []
               , arsNextGPP = [ F.RDI, F.RSI, F.RDX, F.RCX, F.R8, F.R9 ]
               }
   in reverse . arsPrev <$> execStateT m s0


-- | Reserve a 64-bit register for an argument
addGPReg64 :: String -> ArgResolver ()
addGPReg64 nm = ArgResolver $ do
  regs <- gets arsNextGPP
  case regs of
    [] ->
      throwError $ OutOfGPRegs nm
    (r:rest) -> do
      modify $ \s -> s { arsPrev = ArgBV64 r : arsPrev s
                       , arsNextGPP = rest
                       }
