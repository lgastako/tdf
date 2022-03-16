{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Relativ.Multi
  ( MultiIndex
  , build
  ) where

import Relativ.Prelude

-- | A multi-level, or hierarchical Index.
data MultiIndex = MultiIndex

build :: () -> MultiIndex
build = panic "Multi.build"
  where
    _ = MultiIndex
-
