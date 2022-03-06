{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.TimeDelta
  ( TimeDeltaIndex
  , build
  ) where

import Relativ.Prelude

-- | Index of `timedelta64` data.
data TimeDeltaIndex = TimeDeltaIndex

build :: () -> TimeDeltaIndex
build = panic "TimeDeltaIndex.build"
