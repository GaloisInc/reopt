{-|
This contains defines operations to work with the object containing new code.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
module Reopt.Relinker.Object
  ( ObjectSectionIndex
  ) where

import           Data.Word

-- | Identifies this should be a section index in the object file
-- being merged in.
type ObjectSectionIndex = Word16
