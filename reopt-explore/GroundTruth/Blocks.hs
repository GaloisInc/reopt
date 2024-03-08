{- This file was auto-generated from blocks.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module GroundTruth.Blocks (
        BasicBlock(), CalledFunction(), Child(), Function(), Instruction(),
        Module()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
{- | Fields :
     
         * 'Proto.Blocks_Fields.va' @:: Lens' BasicBlock Data.Word.Word64@
         * 'Proto.Blocks_Fields.parent' @:: Lens' BasicBlock Data.Word.Word64@
         * 'Proto.Blocks_Fields.child' @:: Lens' BasicBlock [Child]@
         * 'Proto.Blocks_Fields.vec'child' @:: Lens' BasicBlock (Data.Vector.Vector Child)@
         * 'Proto.Blocks_Fields.instructions' @:: Lens' BasicBlock [Instruction]@
         * 'Proto.Blocks_Fields.vec'instructions' @:: Lens' BasicBlock (Data.Vector.Vector Instruction)@
         * 'Proto.Blocks_Fields.size' @:: Lens' BasicBlock Data.Word.Word32@
         * 'Proto.Blocks_Fields.maybe'size' @:: Lens' BasicBlock (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Blocks_Fields.padding' @:: Lens' BasicBlock Data.Word.Word32@
         * 'Proto.Blocks_Fields.maybe'padding' @:: Lens' BasicBlock (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Blocks_Fields.type'' @:: Lens' BasicBlock Data.Word.Word32@
         * 'Proto.Blocks_Fields.maybe'type'' @:: Lens' BasicBlock (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Blocks_Fields.terminate' @:: Lens' BasicBlock Prelude.Bool@
         * 'Proto.Blocks_Fields.maybe'terminate' @:: Lens' BasicBlock (Prelude.Maybe Prelude.Bool)@ -}
data BasicBlock
  = BasicBlock'_constructor {_BasicBlock'va :: !Data.Word.Word64,
                             _BasicBlock'parent :: !Data.Word.Word64,
                             _BasicBlock'child :: !(Data.Vector.Vector Child),
                             _BasicBlock'instructions :: !(Data.Vector.Vector Instruction),
                             _BasicBlock'size :: !(Prelude.Maybe Data.Word.Word32),
                             _BasicBlock'padding :: !(Prelude.Maybe Data.Word.Word32),
                             _BasicBlock'type' :: !(Prelude.Maybe Data.Word.Word32),
                             _BasicBlock'terminate :: !(Prelude.Maybe Prelude.Bool),
                             _BasicBlock'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BasicBlock where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField BasicBlock "va" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'va (\ x__ y__ -> x__ {_BasicBlock'va = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BasicBlock "parent" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'parent (\ x__ y__ -> x__ {_BasicBlock'parent = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BasicBlock "child" [Child] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'child (\ x__ y__ -> x__ {_BasicBlock'child = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField BasicBlock "vec'child" (Data.Vector.Vector Child) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'child (\ x__ y__ -> x__ {_BasicBlock'child = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BasicBlock "instructions" [Instruction] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'instructions
           (\ x__ y__ -> x__ {_BasicBlock'instructions = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField BasicBlock "vec'instructions" (Data.Vector.Vector Instruction) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'instructions
           (\ x__ y__ -> x__ {_BasicBlock'instructions = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BasicBlock "size" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'size (\ x__ y__ -> x__ {_BasicBlock'size = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField BasicBlock "maybe'size" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'size (\ x__ y__ -> x__ {_BasicBlock'size = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BasicBlock "padding" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'padding (\ x__ y__ -> x__ {_BasicBlock'padding = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField BasicBlock "maybe'padding" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'padding (\ x__ y__ -> x__ {_BasicBlock'padding = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BasicBlock "type'" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'type' (\ x__ y__ -> x__ {_BasicBlock'type' = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField BasicBlock "maybe'type'" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'type' (\ x__ y__ -> x__ {_BasicBlock'type' = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BasicBlock "terminate" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'terminate
           (\ x__ y__ -> x__ {_BasicBlock'terminate = y__}))
        (Data.ProtoLens.maybeLens Prelude.False)
instance Data.ProtoLens.Field.HasField BasicBlock "maybe'terminate" (Prelude.Maybe Prelude.Bool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BasicBlock'terminate
           (\ x__ y__ -> x__ {_BasicBlock'terminate = y__}))
        Prelude.id
instance Data.ProtoLens.Message BasicBlock where
  messageName _ = Data.Text.pack "blocks.BasicBlock"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \BasicBlock\DC2\SO\n\
      \\STXva\CAN\SOH \STX(\EOTR\STXva\DC2\SYN\n\
      \\ACKparent\CAN\STX \STX(\EOTR\ACKparent\DC2#\n\
      \\ENQchild\CAN\ETX \ETX(\v2\r.blocks.ChildR\ENQchild\DC27\n\
      \\finstructions\CAN\EOT \ETX(\v2\DC3.blocks.InstructionR\finstructions\DC2\NAK\n\
      \\EOTsize\CAN\ENQ \SOH(\r:\SOH0R\EOTsize\DC2\ESC\n\
      \\apadding\CAN\ACK \SOH(\r:\SOH0R\apadding\DC2\NAK\n\
      \\EOTtype\CAN\a \SOH(\r:\SOH0R\EOTtype\DC2#\n\
      \\tterminate\CAN\b \SOH(\b:\ENQfalseR\tterminate"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        va__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "va"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Required (Data.ProtoLens.Field.field @"va")) ::
              Data.ProtoLens.FieldDescriptor BasicBlock
        parent__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "parent"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Required (Data.ProtoLens.Field.field @"parent")) ::
              Data.ProtoLens.FieldDescriptor BasicBlock
        child__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "child"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Child)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"child")) ::
              Data.ProtoLens.FieldDescriptor BasicBlock
        instructions__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "instructions"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Instruction)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"instructions")) ::
              Data.ProtoLens.FieldDescriptor BasicBlock
        size__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'size")) ::
              Data.ProtoLens.FieldDescriptor BasicBlock
        padding__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "padding"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'padding")) ::
              Data.ProtoLens.FieldDescriptor BasicBlock
        type'__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'type'")) ::
              Data.ProtoLens.FieldDescriptor BasicBlock
        terminate__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "terminate"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'terminate")) ::
              Data.ProtoLens.FieldDescriptor BasicBlock
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, va__field_descriptor),
           (Data.ProtoLens.Tag 2, parent__field_descriptor),
           (Data.ProtoLens.Tag 3, child__field_descriptor),
           (Data.ProtoLens.Tag 4, instructions__field_descriptor),
           (Data.ProtoLens.Tag 5, size__field_descriptor),
           (Data.ProtoLens.Tag 6, padding__field_descriptor),
           (Data.ProtoLens.Tag 7, type'__field_descriptor),
           (Data.ProtoLens.Tag 8, terminate__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _BasicBlock'_unknownFields
        (\ x__ y__ -> x__ {_BasicBlock'_unknownFields = y__})
  defMessage
    = BasicBlock'_constructor
        {_BasicBlock'va = Data.ProtoLens.fieldDefault,
         _BasicBlock'parent = Data.ProtoLens.fieldDefault,
         _BasicBlock'child = Data.Vector.Generic.empty,
         _BasicBlock'instructions = Data.Vector.Generic.empty,
         _BasicBlock'size = Prelude.Nothing,
         _BasicBlock'padding = Prelude.Nothing,
         _BasicBlock'type' = Prelude.Nothing,
         _BasicBlock'terminate = Prelude.Nothing,
         _BasicBlock'_unknownFields = []}
  parseMessage
    = let
        loop ::
          BasicBlock
          -> Prelude.Bool
             -> Prelude.Bool
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Child
                   -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Instruction
                      -> Data.ProtoLens.Encoding.Bytes.Parser BasicBlock
        loop
          x
          required'parent
          required'va
          mutable'child
          mutable'instructions
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'child <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'child)
                      frozen'instructions <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'instructions)
                      (let
                         missing
                           = (if required'parent then (:) "parent" else Prelude.id)
                               ((if required'va then (:) "va" else Prelude.id) [])
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'child") frozen'child
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'instructions")
                                 frozen'instructions x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "va"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"va") y x)
                                  required'parent Prelude.False mutable'child mutable'instructions
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "parent"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"parent") y x)
                                  Prelude.False required'va mutable'child mutable'instructions
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "child"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'child y)
                                loop x required'parent required'va v mutable'instructions
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "instructions"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'instructions y)
                                loop x required'parent required'va mutable'child v
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "size"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"size") y x)
                                  required'parent required'va mutable'child mutable'instructions
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "padding"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"padding") y x)
                                  required'parent required'va mutable'child mutable'instructions
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"type'") y x)
                                  required'parent required'va mutable'child mutable'instructions
                        64
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "terminate"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"terminate") y x)
                                  required'parent required'va mutable'child mutable'instructions
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  required'parent required'va mutable'child mutable'instructions
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'child <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              mutable'instructions <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage Prelude.True Prelude.True mutable'child
                mutable'instructions)
          "BasicBlock"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                (Data.ProtoLens.Encoding.Bytes.putVarInt
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"va") _x)))
             ((Data.Monoid.<>)
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"parent") _x)))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'child") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage _v))
                         (Lens.Family2.view
                            (Data.ProtoLens.Field.field @"vec'instructions") _x))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'size") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                   ((Prelude..)
                                      Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral
                                      _v))
                         ((Data.Monoid.<>)
                            (case
                                 Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'padding") _x
                             of
                               Prelude.Nothing -> Data.Monoid.mempty
                               (Prelude.Just _v)
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                                      ((Prelude..)
                                         Data.ProtoLens.Encoding.Bytes.putVarInt
                                         Prelude.fromIntegral _v))
                            ((Data.Monoid.<>)
                               (case
                                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'type'") _x
                                of
                                  Prelude.Nothing -> Data.Monoid.mempty
                                  (Prelude.Just _v)
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt 56)
                                         ((Prelude..)
                                            Data.ProtoLens.Encoding.Bytes.putVarInt
                                            Prelude.fromIntegral _v))
                               ((Data.Monoid.<>)
                                  (case
                                       Lens.Family2.view
                                         (Data.ProtoLens.Field.field @"maybe'terminate") _x
                                   of
                                     Prelude.Nothing -> Data.Monoid.mempty
                                     (Prelude.Just _v)
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt 64)
                                            ((Prelude..)
                                               Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (\ b -> if b then 1 else 0) _v))
                                  (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                     (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))))))
instance Control.DeepSeq.NFData BasicBlock where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_BasicBlock'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_BasicBlock'va x__)
                (Control.DeepSeq.deepseq
                   (_BasicBlock'parent x__)
                   (Control.DeepSeq.deepseq
                      (_BasicBlock'child x__)
                      (Control.DeepSeq.deepseq
                         (_BasicBlock'instructions x__)
                         (Control.DeepSeq.deepseq
                            (_BasicBlock'size x__)
                            (Control.DeepSeq.deepseq
                               (_BasicBlock'padding x__)
                               (Control.DeepSeq.deepseq
                                  (_BasicBlock'type' x__)
                                  (Control.DeepSeq.deepseq (_BasicBlock'terminate x__) ()))))))))
{- | Fields :
     
         * 'Proto.Blocks_Fields.va' @:: Lens' CalledFunction Data.Word.Word64@ -}
data CalledFunction
  = CalledFunction'_constructor {_CalledFunction'va :: !Data.Word.Word64,
                                 _CalledFunction'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CalledFunction where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CalledFunction "va" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CalledFunction'va (\ x__ y__ -> x__ {_CalledFunction'va = y__}))
        Prelude.id
instance Data.ProtoLens.Message CalledFunction where
  messageName _ = Data.Text.pack "blocks.CalledFunction"
  packedMessageDescriptor _
    = "\n\
      \\SOCalledFunction\DC2\SO\n\
      \\STXva\CAN\SOH \STX(\EOTR\STXva"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        va__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "va"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Required (Data.ProtoLens.Field.field @"va")) ::
              Data.ProtoLens.FieldDescriptor CalledFunction
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, va__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CalledFunction'_unknownFields
        (\ x__ y__ -> x__ {_CalledFunction'_unknownFields = y__})
  defMessage
    = CalledFunction'_constructor
        {_CalledFunction'va = Data.ProtoLens.fieldDefault,
         _CalledFunction'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CalledFunction
          -> Prelude.Bool
             -> Data.ProtoLens.Encoding.Bytes.Parser CalledFunction
        loop x required'va
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = (if required'va then (:) "va" else Prelude.id) []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "va"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"va") y x)
                                  Prelude.False
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  required'va
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage Prelude.True) "CalledFunction"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                (Data.ProtoLens.Encoding.Bytes.putVarInt
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"va") _x)))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData CalledFunction where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CalledFunction'_unknownFields x__)
             (Control.DeepSeq.deepseq (_CalledFunction'va x__) ())
{- | Fields :
     
         * 'Proto.Blocks_Fields.va' @:: Lens' Child Data.Word.Word64@ -}
data Child
  = Child'_constructor {_Child'va :: !Data.Word.Word64,
                        _Child'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Child where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Child "va" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Child'va (\ x__ y__ -> x__ {_Child'va = y__}))
        Prelude.id
instance Data.ProtoLens.Message Child where
  messageName _ = Data.Text.pack "blocks.Child"
  packedMessageDescriptor _
    = "\n\
      \\ENQChild\DC2\SO\n\
      \\STXva\CAN\SOH \STX(\EOTR\STXva"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        va__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "va"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Required (Data.ProtoLens.Field.field @"va")) ::
              Data.ProtoLens.FieldDescriptor Child
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, va__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Child'_unknownFields
        (\ x__ y__ -> x__ {_Child'_unknownFields = y__})
  defMessage
    = Child'_constructor
        {_Child'va = Data.ProtoLens.fieldDefault,
         _Child'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Child -> Prelude.Bool -> Data.ProtoLens.Encoding.Bytes.Parser Child
        loop x required'va
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = (if required'va then (:) "va" else Prelude.id) []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "va"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"va") y x)
                                  Prelude.False
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  required'va
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage Prelude.True) "Child"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                (Data.ProtoLens.Encoding.Bytes.putVarInt
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"va") _x)))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Child where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Child'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Child'va x__) ())
{- | Fields :
     
         * 'Proto.Blocks_Fields.va' @:: Lens' Function Data.Word.Word64@
         * 'Proto.Blocks_Fields.bb' @:: Lens' Function [BasicBlock]@
         * 'Proto.Blocks_Fields.vec'bb' @:: Lens' Function (Data.Vector.Vector BasicBlock)@
         * 'Proto.Blocks_Fields.calledFunction' @:: Lens' Function [CalledFunction]@
         * 'Proto.Blocks_Fields.vec'calledFunction' @:: Lens' Function (Data.Vector.Vector CalledFunction)@
         * 'Proto.Blocks_Fields.type'' @:: Lens' Function Data.Word.Word32@
         * 'Proto.Blocks_Fields.maybe'type'' @:: Lens' Function (Prelude.Maybe Data.Word.Word32)@ -}
data Function
  = Function'_constructor {_Function'va :: !Data.Word.Word64,
                           _Function'bb :: !(Data.Vector.Vector BasicBlock),
                           _Function'calledFunction :: !(Data.Vector.Vector CalledFunction),
                           _Function'type' :: !(Prelude.Maybe Data.Word.Word32),
                           _Function'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Function where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Function "va" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'va (\ x__ y__ -> x__ {_Function'va = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Function "bb" [BasicBlock] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'bb (\ x__ y__ -> x__ {_Function'bb = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Function "vec'bb" (Data.Vector.Vector BasicBlock) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'bb (\ x__ y__ -> x__ {_Function'bb = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Function "calledFunction" [CalledFunction] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'calledFunction
           (\ x__ y__ -> x__ {_Function'calledFunction = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Function "vec'calledFunction" (Data.Vector.Vector CalledFunction) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'calledFunction
           (\ x__ y__ -> x__ {_Function'calledFunction = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Function "type'" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'type' (\ x__ y__ -> x__ {_Function'type' = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField Function "maybe'type'" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'type' (\ x__ y__ -> x__ {_Function'type' = y__}))
        Prelude.id
instance Data.ProtoLens.Message Function where
  messageName _ = Data.Text.pack "blocks.Function"
  packedMessageDescriptor _
    = "\n\
      \\bFunction\DC2\SO\n\
      \\STXva\CAN\SOH \STX(\EOTR\STXva\DC2\"\n\
      \\STXbb\CAN\STX \ETX(\v2\DC2.blocks.BasicBlockR\STXbb\DC2>\n\
      \\SOcalledFunction\CAN\ETX \ETX(\v2\SYN.blocks.CalledFunctionR\SOcalledFunction\DC2\NAK\n\
      \\EOTtype\CAN\EOT \SOH(\r:\SOH0R\EOTtype"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        va__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "va"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Required (Data.ProtoLens.Field.field @"va")) ::
              Data.ProtoLens.FieldDescriptor Function
        bb__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "bb"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BasicBlock)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"bb")) ::
              Data.ProtoLens.FieldDescriptor Function
        calledFunction__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "calledFunction"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CalledFunction)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"calledFunction")) ::
              Data.ProtoLens.FieldDescriptor Function
        type'__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'type'")) ::
              Data.ProtoLens.FieldDescriptor Function
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, va__field_descriptor),
           (Data.ProtoLens.Tag 2, bb__field_descriptor),
           (Data.ProtoLens.Tag 3, calledFunction__field_descriptor),
           (Data.ProtoLens.Tag 4, type'__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Function'_unknownFields
        (\ x__ y__ -> x__ {_Function'_unknownFields = y__})
  defMessage
    = Function'_constructor
        {_Function'va = Data.ProtoLens.fieldDefault,
         _Function'bb = Data.Vector.Generic.empty,
         _Function'calledFunction = Data.Vector.Generic.empty,
         _Function'type' = Prelude.Nothing, _Function'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Function
          -> Prelude.Bool
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld BasicBlock
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld CalledFunction
                   -> Data.ProtoLens.Encoding.Bytes.Parser Function
        loop x required'va mutable'bb mutable'calledFunction
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'bb <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'bb)
                      frozen'calledFunction <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'calledFunction)
                      (let missing = (if required'va then (:) "va" else Prelude.id) []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'bb") frozen'bb
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'calledFunction")
                                 frozen'calledFunction x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "va"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"va") y x)
                                  Prelude.False mutable'bb mutable'calledFunction
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "bb"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'bb y)
                                loop x required'va v mutable'calledFunction
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "calledFunction"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'calledFunction y)
                                loop x required'va mutable'bb v
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"type'") y x)
                                  required'va mutable'bb mutable'calledFunction
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  required'va mutable'bb mutable'calledFunction
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'bb <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                              Data.ProtoLens.Encoding.Growing.new
              mutable'calledFunction <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage Prelude.True mutable'bb
                mutable'calledFunction)
          "Function"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                (Data.ProtoLens.Encoding.Bytes.putVarInt
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"va") _x)))
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'bb") _x))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'calledFunction") _x))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'type'") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                                ((Prelude..)
                                   Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData Function where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Function'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Function'va x__)
                (Control.DeepSeq.deepseq
                   (_Function'bb x__)
                   (Control.DeepSeq.deepseq
                      (_Function'calledFunction x__)
                      (Control.DeepSeq.deepseq (_Function'type' x__) ()))))
{- | Fields :
     
         * 'Proto.Blocks_Fields.va' @:: Lens' Instruction Data.Word.Word64@
         * 'Proto.Blocks_Fields.size' @:: Lens' Instruction Data.Word.Word32@
         * 'Proto.Blocks_Fields.maybe'size' @:: Lens' Instruction (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Blocks_Fields.callType' @:: Lens' Instruction Data.Word.Word32@
         * 'Proto.Blocks_Fields.maybe'callType' @:: Lens' Instruction (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Blocks_Fields.callee' @:: Lens' Instruction Data.Word.Word64@
         * 'Proto.Blocks_Fields.maybe'callee' @:: Lens' Instruction (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Blocks_Fields.calleeName' @:: Lens' Instruction Data.Text.Text@
         * 'Proto.Blocks_Fields.maybe'calleeName' @:: Lens' Instruction (Prelude.Maybe Data.Text.Text)@ -}
data Instruction
  = Instruction'_constructor {_Instruction'va :: !Data.Word.Word64,
                              _Instruction'size :: !(Prelude.Maybe Data.Word.Word32),
                              _Instruction'callType :: !(Prelude.Maybe Data.Word.Word32),
                              _Instruction'callee :: !(Prelude.Maybe Data.Word.Word64),
                              _Instruction'calleeName :: !(Prelude.Maybe Data.Text.Text),
                              _Instruction'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Instruction where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Instruction "va" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'va (\ x__ y__ -> x__ {_Instruction'va = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Instruction "size" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'size (\ x__ y__ -> x__ {_Instruction'size = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField Instruction "maybe'size" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'size (\ x__ y__ -> x__ {_Instruction'size = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Instruction "callType" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'callType
           (\ x__ y__ -> x__ {_Instruction'callType = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField Instruction "maybe'callType" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'callType
           (\ x__ y__ -> x__ {_Instruction'callType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Instruction "callee" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'callee (\ x__ y__ -> x__ {_Instruction'callee = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField Instruction "maybe'callee" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'callee (\ x__ y__ -> x__ {_Instruction'callee = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Instruction "calleeName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'calleeName
           (\ x__ y__ -> x__ {_Instruction'calleeName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField Instruction "maybe'calleeName" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Instruction'calleeName
           (\ x__ y__ -> x__ {_Instruction'calleeName = y__}))
        Prelude.id
instance Data.ProtoLens.Message Instruction where
  messageName _ = Data.Text.pack "blocks.Instruction"
  packedMessageDescriptor _
    = "\n\
      \\vInstruction\DC2\SO\n\
      \\STXva\CAN\SOH \STX(\EOTR\STXva\DC2\NAK\n\
      \\EOTsize\CAN\STX \SOH(\r:\SOH0R\EOTsize\DC2\RS\n\
      \\tcall_type\CAN\ETX \SOH(\r:\SOH0R\bcallType\DC2\EM\n\
      \\ACKcallee\CAN\EOT \SOH(\EOT:\SOH0R\ACKcallee\DC2!\n\
      \\vcallee_name\CAN\ENQ \SOH(\t:\NULR\n\
      \calleeName"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        va__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "va"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Required (Data.ProtoLens.Field.field @"va")) ::
              Data.ProtoLens.FieldDescriptor Instruction
        size__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'size")) ::
              Data.ProtoLens.FieldDescriptor Instruction
        callType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "call_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'callType")) ::
              Data.ProtoLens.FieldDescriptor Instruction
        callee__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "callee"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'callee")) ::
              Data.ProtoLens.FieldDescriptor Instruction
        calleeName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "callee_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'calleeName")) ::
              Data.ProtoLens.FieldDescriptor Instruction
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, va__field_descriptor),
           (Data.ProtoLens.Tag 2, size__field_descriptor),
           (Data.ProtoLens.Tag 3, callType__field_descriptor),
           (Data.ProtoLens.Tag 4, callee__field_descriptor),
           (Data.ProtoLens.Tag 5, calleeName__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Instruction'_unknownFields
        (\ x__ y__ -> x__ {_Instruction'_unknownFields = y__})
  defMessage
    = Instruction'_constructor
        {_Instruction'va = Data.ProtoLens.fieldDefault,
         _Instruction'size = Prelude.Nothing,
         _Instruction'callType = Prelude.Nothing,
         _Instruction'callee = Prelude.Nothing,
         _Instruction'calleeName = Prelude.Nothing,
         _Instruction'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Instruction
          -> Prelude.Bool -> Data.ProtoLens.Encoding.Bytes.Parser Instruction
        loop x required'va
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = (if required'va then (:) "va" else Prelude.id) []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "va"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"va") y x)
                                  Prelude.False
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "size"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"size") y x)
                                  required'va
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "call_type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"callType") y x)
                                  required'va
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "callee"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"callee") y x)
                                  required'va
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "callee_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"calleeName") y x)
                                  required'va
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  required'va
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage Prelude.True) "Instruction"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                (Data.ProtoLens.Encoding.Bytes.putVarInt
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"va") _x)))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'size") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                          ((Prelude..)
                             Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'callType") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                             ((Prelude..)
                                Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'callee") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view
                                (Data.ProtoLens.Field.field @"maybe'calleeName") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.Text.Encoding.encodeUtf8 _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData Instruction where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Instruction'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Instruction'va x__)
                (Control.DeepSeq.deepseq
                   (_Instruction'size x__)
                   (Control.DeepSeq.deepseq
                      (_Instruction'callType x__)
                      (Control.DeepSeq.deepseq
                         (_Instruction'callee x__)
                         (Control.DeepSeq.deepseq (_Instruction'calleeName x__) ())))))
{- | Fields :
     
         * 'Proto.Blocks_Fields.fuc' @:: Lens' Module [Function]@
         * 'Proto.Blocks_Fields.vec'fuc' @:: Lens' Module (Data.Vector.Vector Function)@
         * 'Proto.Blocks_Fields.textStart' @:: Lens' Module Data.Word.Word64@
         * 'Proto.Blocks_Fields.maybe'textStart' @:: Lens' Module (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Blocks_Fields.textEnd' @:: Lens' Module Data.Word.Word64@
         * 'Proto.Blocks_Fields.maybe'textEnd' @:: Lens' Module (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Blocks_Fields.splitBlock' @:: Lens' Module Prelude.Bool@
         * 'Proto.Blocks_Fields.maybe'splitBlock' @:: Lens' Module (Prelude.Maybe Prelude.Bool)@ -}
data Module
  = Module'_constructor {_Module'fuc :: !(Data.Vector.Vector Function),
                         _Module'textStart :: !(Prelude.Maybe Data.Word.Word64),
                         _Module'textEnd :: !(Prelude.Maybe Data.Word.Word64),
                         _Module'splitBlock :: !(Prelude.Maybe Prelude.Bool),
                         _Module'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Module where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Module "fuc" [Function] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'fuc (\ x__ y__ -> x__ {_Module'fuc = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Module "vec'fuc" (Data.Vector.Vector Function) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'fuc (\ x__ y__ -> x__ {_Module'fuc = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Module "textStart" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'textStart (\ x__ y__ -> x__ {_Module'textStart = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField Module "maybe'textStart" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'textStart (\ x__ y__ -> x__ {_Module'textStart = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Module "textEnd" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'textEnd (\ x__ y__ -> x__ {_Module'textEnd = y__}))
        (Data.ProtoLens.maybeLens 0)
instance Data.ProtoLens.Field.HasField Module "maybe'textEnd" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'textEnd (\ x__ y__ -> x__ {_Module'textEnd = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Module "splitBlock" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'splitBlock (\ x__ y__ -> x__ {_Module'splitBlock = y__}))
        (Data.ProtoLens.maybeLens Prelude.False)
instance Data.ProtoLens.Field.HasField Module "maybe'splitBlock" (Prelude.Maybe Prelude.Bool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'splitBlock (\ x__ y__ -> x__ {_Module'splitBlock = y__}))
        Prelude.id
instance Data.ProtoLens.Message Module where
  messageName _ = Data.Text.pack "blocks.module"
  packedMessageDescriptor _
    = "\n\
      \\ACKmodule\DC2\"\n\
      \\ETXfuc\CAN\SOH \ETX(\v2\DLE.blocks.FunctionR\ETXfuc\DC2 \n\
      \\n\
      \text_start\CAN\STX \SOH(\EOT:\SOH0R\ttextStart\DC2\FS\n\
      \\btext_end\CAN\ETX \SOH(\EOT:\SOH0R\atextEnd\DC2&\n\
      \\vsplit_block\CAN\EOT \SOH(\b:\ENQfalseR\n\
      \splitBlock"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fuc__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fuc"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Function)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"fuc")) ::
              Data.ProtoLens.FieldDescriptor Module
        textStart__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "text_start"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'textStart")) ::
              Data.ProtoLens.FieldDescriptor Module
        textEnd__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "text_end"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'textEnd")) ::
              Data.ProtoLens.FieldDescriptor Module
        splitBlock__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "split_block"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'splitBlock")) ::
              Data.ProtoLens.FieldDescriptor Module
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fuc__field_descriptor),
           (Data.ProtoLens.Tag 2, textStart__field_descriptor),
           (Data.ProtoLens.Tag 3, textEnd__field_descriptor),
           (Data.ProtoLens.Tag 4, splitBlock__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Module'_unknownFields
        (\ x__ y__ -> x__ {_Module'_unknownFields = y__})
  defMessage
    = Module'_constructor
        {_Module'fuc = Data.Vector.Generic.empty,
         _Module'textStart = Prelude.Nothing,
         _Module'textEnd = Prelude.Nothing,
         _Module'splitBlock = Prelude.Nothing, _Module'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Module
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Function
             -> Data.ProtoLens.Encoding.Bytes.Parser Module
        loop x mutable'fuc
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'fuc <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'fuc)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'fuc") frozen'fuc x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "fuc"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'fuc y)
                                loop x v
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "text_start"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"textStart") y x)
                                  mutable'fuc
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "text_end"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"textEnd") y x)
                                  mutable'fuc
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "split_block"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"splitBlock") y x)
                                  mutable'fuc
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'fuc
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'fuc <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                               Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'fuc)
          "module"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'fuc") _x))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'textStart") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'textEnd") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'splitBlock") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                                ((Prelude..)
                                   Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (\ b -> if b then 1 else 0) _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData Module where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Module'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Module'fuc x__)
                (Control.DeepSeq.deepseq
                   (_Module'textStart x__)
                   (Control.DeepSeq.deepseq
                      (_Module'textEnd x__)
                      (Control.DeepSeq.deepseq (_Module'splitBlock x__) ()))))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\fblocks.proto\DC2\ACKblocks\"\148\SOH\n\
    \\ACKmodule\DC2\"\n\
    \\ETXfuc\CAN\SOH \ETX(\v2\DLE.blocks.FunctionR\ETXfuc\DC2 \n\
    \\n\
    \text_start\CAN\STX \SOH(\EOT:\SOH0R\ttextStart\DC2\FS\n\
    \\btext_end\CAN\ETX \SOH(\EOT:\SOH0R\atextEnd\DC2&\n\
    \\vsplit_block\CAN\EOT \SOH(\b:\ENQfalseR\n\
    \splitBlock\"\149\SOH\n\
    \\bFunction\DC2\SO\n\
    \\STXva\CAN\SOH \STX(\EOTR\STXva\DC2\"\n\
    \\STXbb\CAN\STX \ETX(\v2\DC2.blocks.BasicBlockR\STXbb\DC2>\n\
    \\SOcalledFunction\CAN\ETX \ETX(\v2\SYN.blocks.CalledFunctionR\SOcalledFunction\DC2\NAK\n\
    \\EOTtype\CAN\EOT \SOH(\r:\SOH0R\EOTtype\"\ETB\n\
    \\ENQChild\DC2\SO\n\
    \\STXva\CAN\SOH \STX(\EOTR\STXva\"\146\SOH\n\
    \\vInstruction\DC2\SO\n\
    \\STXva\CAN\SOH \STX(\EOTR\STXva\DC2\NAK\n\
    \\EOTsize\CAN\STX \SOH(\r:\SOH0R\EOTsize\DC2\RS\n\
    \\tcall_type\CAN\ETX \SOH(\r:\SOH0R\bcallType\DC2\EM\n\
    \\ACKcallee\CAN\EOT \SOH(\EOT:\SOH0R\ACKcallee\DC2!\n\
    \\vcallee_name\CAN\ENQ \SOH(\t:\NULR\n\
    \calleeName\" \n\
    \\SOCalledFunction\DC2\SO\n\
    \\STXva\CAN\SOH \STX(\EOTR\STXva\"\130\STX\n\
    \\n\
    \BasicBlock\DC2\SO\n\
    \\STXva\CAN\SOH \STX(\EOTR\STXva\DC2\SYN\n\
    \\ACKparent\CAN\STX \STX(\EOTR\ACKparent\DC2#\n\
    \\ENQchild\CAN\ETX \ETX(\v2\r.blocks.ChildR\ENQchild\DC27\n\
    \\finstructions\CAN\EOT \ETX(\v2\DC3.blocks.InstructionR\finstructions\DC2\NAK\n\
    \\EOTsize\CAN\ENQ \SOH(\r:\SOH0R\EOTsize\DC2\ESC\n\
    \\apadding\CAN\ACK \SOH(\r:\SOH0R\apadding\DC2\NAK\n\
    \\EOTtype\CAN\a \SOH(\r:\SOH0R\EOTtype\DC2#\n\
    \\tterminate\CAN\b \SOH(\b:\ENQfalseR\tterminateJ\130\SYN\n\
    \\ACK\DC2\EOT\NUL\NUL<\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\SI\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\EOT\NUL\t\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\EOT\b\SO\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\ENQ\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETX\ENQ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\ENQ\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\ENQ\DC4\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\ENQ\SUB\ESC\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\ACK\STX/\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\EOT\DC2\ETX\ACK\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX\ACK\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\ACK\DC2\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\ACK\US \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\b\DC2\ETX\ACK!.\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\a\DC2\ETX\ACK,-\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX\a\STX-\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\EOT\DC2\ETX\a\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ENQ\DC2\ETX\a\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX\a\DC2\SUB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX\a\GS\RS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\b\DC2\ETX\a\US,\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\a\DC2\ETX\a*+\n\
    \:\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETX\b\STX2\"- split the basic block by `call` instruction\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\EOT\DC2\ETX\b\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ENQ\DC2\ETX\b\v\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETX\b\DLE\ESC\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETX\b\RS\US\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\b\DC2\ETX\b 1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\a\DC2\ETX\b+0\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\v\NUL\DC3\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\v\b\DLE\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\f\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\EOT\DC2\ETX\f\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\f\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\f\DC2\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\f\ETB\CAN\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\r\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\EOT\DC2\ETX\r\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX\r\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\r\SYN\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\r\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX\SO\STX,\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\EOT\DC2\ETX\SO\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ACK\DC2\ETX\SO\v\EM\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX\SO\SUB(\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX\SO*+\n\
    \8\n\
    \\EOT\EOT\SOH\STX\ETX\DC2\ETX\SI\STX)\"+ 0 represents that the function is normal.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\EOT\DC2\ETX\SI\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\ENQ\DC2\ETX\SI\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\SOH\DC2\ETX\SI\DC2\SYN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\ETX\DC2\ETX\SI\EM\SUB\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\b\DC2\ETX\SI\ESC(\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\a\DC2\ETX\SI&'\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\NAK\NUL\ETB\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\NAK\b\r\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\SYN\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\EOT\DC2\ETX\SYN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\SYN\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\SYN\DC2\DC4\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\SYN\ETB\CAN\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT\EM\NUL\US\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\EM\b\DC3\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\SUB\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\EOT\DC2\ETX\SUB\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX\SUB\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\SUB\DC2\DC4\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\SUB\ETB\CAN\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETX\ESC\STX)\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\EOT\DC2\ETX\ESC\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\ETX\ESC\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETX\ESC\DC2\SYN\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETX\ESC\EM\SUB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\b\DC2\ETX\ESC\ESC(\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\a\DC2\ETX\ESC&'\n\
    \W\n\
    \\EOT\EOT\ETX\STX\STX\DC2\ETX\FS\STX.\"J 1 is direct call or indirect call, 2 is indirect call, 3 is direct call.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\EOT\DC2\ETX\FS\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ENQ\DC2\ETX\FS\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\ETX\FS\DC2\ESC\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\ETX\FS\RS\US\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\b\DC2\ETX\FS -\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\a\DC2\ETX\FS+,\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\ETX\DC2\ETX\GS\STX+\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\EOT\DC2\ETX\GS\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\ENQ\DC2\ETX\GS\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\SOH\DC2\ETX\GS\DC2\CAN\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\ETX\DC2\ETX\GS\ESC\FS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\b\DC2\ETX\GS\GS*\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\a\DC2\ETX\GS()\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\EOT\DC2\ETX\RS\STX1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\EOT\DC2\ETX\RS\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\ENQ\DC2\ETX\RS\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\SOH\DC2\ETX\RS\DC2\GS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\ETX\DC2\ETX\RS !\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\b\DC2\ETX\RS\"0\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\a\DC2\ETX\RS-/\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT!\NUL#\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX!\b\SYN\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX\"\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\EOT\DC2\ETX\"\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETX\"\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX\"\DC2\DC4\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX\"\ETB\CAN\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOT%\NUL<\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX%\b\DC2\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX&\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\EOT\DC2\ETX&\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ENQ\DC2\ETX&\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX&\DC2\DC4\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX&\ETB\CAN\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\ETX'\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\EOT\DC2\ETX'\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ENQ\DC2\ETX'\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\ETX'\DC2\CAN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\ETX'\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\STX\DC2\ETX(\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\EOT\DC2\ETX(\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ACK\DC2\ETX(\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\SOH\DC2\ETX(\DC1\SYN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ETX\DC2\ETX(\EM\SUB\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\ETX\DC2\ETX)\STX(\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\EOT\DC2\ETX)\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\ACK\DC2\ETX)\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\SOH\DC2\ETX)\ETB#\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\ETX\DC2\ETX)&'\n\
    \;\n\
    \\EOT\EOT\ENQ\STX\EOT\DC2\ETX*\STX)\". basic block actual size, not include padding\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\EOT\DC2\ETX*\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\ENQ\DC2\ETX*\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\SOH\DC2\ETX*\DC2\SYN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\ETX\DC2\ETX*\EM\SUB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\b\DC2\ETX*\ESC(\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\a\DC2\ETX*&'\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\ENQ\DC2\ETX+\STX,\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\EOT\DC2\ETX+\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\ENQ\DC2\ETX+\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\SOH\DC2\ETX+\DC2\EM\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\ETX\DC2\ETX+\FS\GS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\b\DC2\ETX+\RS+\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\a\DC2\ETX+)*\n\
    \\EM\n\
    \\EOT\EOT\ENQ\STX\ACK\DC2\ETX,\STX)\"\f other type\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\EOT\DC2\ETX,\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\ENQ\DC2\ETX,\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\SOH\DC2\ETX,\DC2\SYN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\ETX\DC2\ETX,\EM\SUB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\b\DC2\ETX,\ESC(\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ACK\a\DC2\ETX,&'\n\
    \\247\ETX\n\
    \\EOT\EOT\ENQ\STX\a\DC2\ETX;\SOH/\SUB\173\ETX direct call instruction\n\
    \ indirect call instruction\n\
    \ret instruction\n\
    \conditional jump(direct)\n\
    \direct jump\n\
    \indirect jump\n\
    \jump table\n\
    \non-return function call\n\
    \fall_through\n\
    \overlapping instruction(not used)\n\
    \tail call\n\
    \ fall through to another function. these two functin share some codes\n\
    \ jump to another function start, but in current functin range. that is                       these two function share some codes\n\
    \ dummy jump table\n\
    \\": does this block contains terminate instruction, like ud2\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\EOT\DC2\ETX;\SOH\t\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\ENQ\DC2\ETX;\n\
    \\SO\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\SOH\DC2\ETX;\SI\CAN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\ETX\DC2\ETX;\ESC\FS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\b\DC2\ETX;\GS.\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\a\a\DC2\ETX;(-"
