{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Residual.Recognizers where

import qualified Data.ByteString.Char8            as BSC
import           Data.Macaw.Analysis.FunctionArgs (FunctionArgAnalysisFailure (CallAnalysisError, PLTStubNotSupported))
import           Data.Maybe                       (fromJust)
import           Flexdis86                        (DisassembledAddr (disInstruction),
                                                   InstructionInstanceF (iiArgs, iiOp),
                                                   JumpSize (JSize8),
                                                   Value (DWordImm, DWordReg, JumpOffset, QWordReg, WordReg),
                                                   pattern AX, pattern EAX,
                                                   pattern EDI, pattern RAX)

data ResidualExplanation
  = DeregisterTmClones
  | NopPadding
  | RegisterTmClones
  | BecauseFailure (FunctionArgAnalysisFailure 64)

ppResidualExplanation :: ResidualExplanation -> String
ppResidualExplanation = \case
  DeregisterTmClones                               -> "deregister_tm_clones"
  NopPadding                                       -> "nop padding"
  RegisterTmClones                                 -> "register_tm_clones"
  BecauseFailure PLTStubNotSupported               -> "PLT stub not supported"
  BecauseFailure (CallAnalysisError _callSite msg) -> msg

classifyInstrs :: [DisassembledAddr] -> Maybe ResidualExplanation
classifyInstrs instrs
  | isNopPadding instrs = Just NopPadding
  | isDeregisterTmClonesSuffix instrs = Just DeregisterTmClones
  | isRegisterTmClonesSuffix instrs = Just RegisterTmClones
  | otherwise = Nothing

inspectInstr :: DisassembledAddr -> (String, [Value])
inspectInstr addr =
  let i = fromJust $ disInstruction addr in
  (BSC.unpack $ iiOp i, fst <$> iiArgs i)

isNopInstr :: DisassembledAddr -> Bool
isNopInstr addr =
  case inspectInstr addr of
    ("nop", _)                             -> True
    ("xchg", [WordReg AX, WordReg AX])     -> True
    ("xchg", [DWordReg EAX, DWordReg EAX]) -> True
    _                                      -> False

isNopPadding :: [DisassembledAddr] -> Bool
isNopPadding = all isNopInstr

isDeregisterTmClonesSuffix :: [DisassembledAddr] -> Bool
isDeregisterTmClonesSuffix
  [ inspectInstr -> ("mov", [DWordReg EAX, DWordImm _])
  , inspectInstr -> ("test", [QWordReg RAX, QWordReg RAX])
  , inspectInstr -> ("je", [JumpOffset JSize8 _])
  , inspectInstr -> ("mov", [DWordReg EDI, DWordImm _])
  , inspectInstr -> ("jmp", [QWordReg RAX])
  , isNopInstr -> True
  ] = True
isDeregisterTmClonesSuffix _ = False

isRegisterTmClonesSuffix :: [DisassembledAddr] -> Bool
isRegisterTmClonesSuffix
  [ inspectInstr -> ("mov", [DWordReg EAX, DWordImm _])
  , inspectInstr -> ("test", [QWordReg RAX, QWordReg RAX])
  , inspectInstr -> ("je", [JumpOffset JSize8 _])
  , inspectInstr -> ("mov", [DWordReg EDI, DWordImm _])
  , inspectInstr -> ("jmp", [QWordReg RAX])
  ] = True
isRegisterTmClonesSuffix _ = False
