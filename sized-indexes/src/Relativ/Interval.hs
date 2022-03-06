{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Interval
  ( IntervalIndex
  , build
  ) where

import Relativ.Prelude

import Relativ.Types.Closedness ( Closedness )
import Relativ.Types.Interval   ( Interval )

-- | An Index of `Interval`s.
data IntervalIndex (c :: Closedness) = IntervalIndex
  {
  }

build :: Vec n (Interval c a)
      -> IntervalIndex c
build = panic "Interval.build"
  where
    _x_ = IntervalIndex
