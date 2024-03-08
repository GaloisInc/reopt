{- This file was auto-generated from blocks.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module GroundTruth.Blocks_Fields where
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
bb ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "bb" a) =>
  Lens.Family2.LensLike' f s a
bb = Data.ProtoLens.Field.field @"bb"
callType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "callType" a) =>
  Lens.Family2.LensLike' f s a
callType = Data.ProtoLens.Field.field @"callType"
calledFunction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "calledFunction" a) =>
  Lens.Family2.LensLike' f s a
calledFunction = Data.ProtoLens.Field.field @"calledFunction"
callee ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "callee" a) =>
  Lens.Family2.LensLike' f s a
callee = Data.ProtoLens.Field.field @"callee"
calleeName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "calleeName" a) =>
  Lens.Family2.LensLike' f s a
calleeName = Data.ProtoLens.Field.field @"calleeName"
child ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "child" a) =>
  Lens.Family2.LensLike' f s a
child = Data.ProtoLens.Field.field @"child"
fuc ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "fuc" a) =>
  Lens.Family2.LensLike' f s a
fuc = Data.ProtoLens.Field.field @"fuc"
instructions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "instructions" a) =>
  Lens.Family2.LensLike' f s a
instructions = Data.ProtoLens.Field.field @"instructions"
maybe'callType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'callType" a) =>
  Lens.Family2.LensLike' f s a
maybe'callType = Data.ProtoLens.Field.field @"maybe'callType"
maybe'callee ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'callee" a) =>
  Lens.Family2.LensLike' f s a
maybe'callee = Data.ProtoLens.Field.field @"maybe'callee"
maybe'calleeName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'calleeName" a) =>
  Lens.Family2.LensLike' f s a
maybe'calleeName = Data.ProtoLens.Field.field @"maybe'calleeName"
maybe'padding ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'padding" a) =>
  Lens.Family2.LensLike' f s a
maybe'padding = Data.ProtoLens.Field.field @"maybe'padding"
maybe'size ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'size" a) =>
  Lens.Family2.LensLike' f s a
maybe'size = Data.ProtoLens.Field.field @"maybe'size"
maybe'splitBlock ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'splitBlock" a) =>
  Lens.Family2.LensLike' f s a
maybe'splitBlock = Data.ProtoLens.Field.field @"maybe'splitBlock"
maybe'terminate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'terminate" a) =>
  Lens.Family2.LensLike' f s a
maybe'terminate = Data.ProtoLens.Field.field @"maybe'terminate"
maybe'textEnd ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'textEnd" a) =>
  Lens.Family2.LensLike' f s a
maybe'textEnd = Data.ProtoLens.Field.field @"maybe'textEnd"
maybe'textStart ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'textStart" a) =>
  Lens.Family2.LensLike' f s a
maybe'textStart = Data.ProtoLens.Field.field @"maybe'textStart"
maybe'type' ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'type'" a) =>
  Lens.Family2.LensLike' f s a
maybe'type' = Data.ProtoLens.Field.field @"maybe'type'"
padding ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "padding" a) =>
  Lens.Family2.LensLike' f s a
padding = Data.ProtoLens.Field.field @"padding"
parent ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "parent" a) =>
  Lens.Family2.LensLike' f s a
parent = Data.ProtoLens.Field.field @"parent"
size ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "size" a) =>
  Lens.Family2.LensLike' f s a
size = Data.ProtoLens.Field.field @"size"
splitBlock ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "splitBlock" a) =>
  Lens.Family2.LensLike' f s a
splitBlock = Data.ProtoLens.Field.field @"splitBlock"
terminate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "terminate" a) =>
  Lens.Family2.LensLike' f s a
terminate = Data.ProtoLens.Field.field @"terminate"
textEnd ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "textEnd" a) =>
  Lens.Family2.LensLike' f s a
textEnd = Data.ProtoLens.Field.field @"textEnd"
textStart ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "textStart" a) =>
  Lens.Family2.LensLike' f s a
textStart = Data.ProtoLens.Field.field @"textStart"
type' ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "type'" a) =>
  Lens.Family2.LensLike' f s a
type' = Data.ProtoLens.Field.field @"type'"
va ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "va" a) =>
  Lens.Family2.LensLike' f s a
va = Data.ProtoLens.Field.field @"va"
vec'bb ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'bb" a) =>
  Lens.Family2.LensLike' f s a
vec'bb = Data.ProtoLens.Field.field @"vec'bb"
vec'calledFunction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'calledFunction" a) =>
  Lens.Family2.LensLike' f s a
vec'calledFunction
  = Data.ProtoLens.Field.field @"vec'calledFunction"
vec'child ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'child" a) =>
  Lens.Family2.LensLike' f s a
vec'child = Data.ProtoLens.Field.field @"vec'child"
vec'fuc ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'fuc" a) =>
  Lens.Family2.LensLike' f s a
vec'fuc = Data.ProtoLens.Field.field @"vec'fuc"
vec'instructions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'instructions" a) =>
  Lens.Family2.LensLike' f s a
vec'instructions = Data.ProtoLens.Field.field @"vec'instructions"
