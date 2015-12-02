
module Reopt.Machine.SysDeps.Types where

data SyscallArgType = VoidArgType | WordArgType | XMMFloatType
                    deriving (Eq, Show, Read)

type SyscallTypeInfo = (String, SyscallArgType, [SyscallArgType])
