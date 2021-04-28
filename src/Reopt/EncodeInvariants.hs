{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.EncodeInvariants
  ( encodeInvariantMsg
  , encodeInvariantFailedMsg
  ) where

import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Some
import qualified Data.Text as Text
import           Data.Word
import           Numeric (showHex)

import           Data.Macaw.AbsDomain.StackAnalysis
import           Data.Macaw.Analysis.RegisterUse
import           Data.Macaw.Memory
import           Data.Macaw.X86

encodeAddr :: Word64 -> Aeson.Encoding
encodeAddr a = Aeson.text (Text.pack (showHex a ""))

encodeSegmentOff :: MemSegmentOff  w -> Aeson.Encoding
encodeSegmentOff a = encodeAddr (memWordValue (addrOffset (segoffAddr a)))

encodeSomePhiLoc :: Some (BoundLoc X86Reg) -> Aeson.Encoding
encodeSomePhiLoc (Some (RegLoc r)) = Aeson.text (Text.pack (show r))
encodeSomePhiLoc (Some (StackOffLoc o repr)) =
  Aeson.pairs $ Aeson.pair "off" (Aeson.int64 (memIntValue o))
             <> Aeson.pair "repr" (Aeson.text (Text.pack (show repr)))

encodeBlockInvariants :: MemSegmentOff 64 -> BlockInvariants X86_64 ids -> Aeson.Encoding
encodeBlockInvariants addr inv = Aeson.pairs $
     Aeson.pair "addr" (encodeSegmentOff addr)
--  <> Aeson.pair "loc-map" !(MapF (BoundLoc (ArchReg arch)) (LocList (ArchReg arch)))
  <> Aeson.pair "phi-location" (Aeson.list encodeSomePhiLoc (biPhiLocs inv))

encodeBlockInvariantMap :: BlockInvariantMap X86_64 ids -> Aeson.Encoding
encodeBlockInvariantMap m = Aeson.list (uncurry encodeBlockInvariants) (Map.toList m)

encodeInvariantMsg :: Word64 -> BlockInvariantMap X86_64 ids -> Aeson.Encoding
encodeInvariantMsg addr m =
  Aeson.pairs $
    Aeson.pair "type" (Aeson.text "invariant")
    <> Aeson.pair "addr" (encodeAddr addr)
    <> Aeson.pair "invariants" (encodeBlockInvariantMap m)

encodeInvariantFailedMsg :: Word64 -> String -> Aeson.Encoding
encodeInvariantFailedMsg addr msg =
  Aeson.pairs $
    Aeson.pair "type" (Aeson.text "invariantFailed")
    <> Aeson.pair "addr" (encodeAddr addr)
    <> Aeson.pair "message" (Aeson.text (Text.pack msg))