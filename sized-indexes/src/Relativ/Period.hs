{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Period
  ( PeriodIndex
  , build
  ) where

import Relativ.Prelude

-- | Index of `Period` data.
data PeriodIndex = PeriodIndex

build :: () -> PeriodIndex
build = panic "Period.build"
  where
    _ = PeriodIndex
