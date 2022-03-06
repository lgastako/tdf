{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Interval
  ( IntervalIndex
  ) where

import Relativ.Prelude

import Relativ.Types.Closedness ( Closedness )
import Relativ.Types.Interval   ( Interval )

-- | An Index of `Interval`s.
data IntervalIndex = IntervalIndex
