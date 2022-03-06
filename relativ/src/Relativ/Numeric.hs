{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Relativ.Numeric
  ( NumericIndex
  , build
  ) where

import Relativ.Prelude

-- | Index of `int`/`uint`/`float` data.
data NumericIndex = NumericIndex

build :: () -> NumericIndex
build = panic "Numeric.build"
  where
    _ = NumericIndex
