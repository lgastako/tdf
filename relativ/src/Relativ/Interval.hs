{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.Interval
  ( IntervalIndex
  , build
  ) where

import Relativ.Prelude

import Relativ.Types.Openness ( Openness )
import Relativ.Types.Interval ( Interval )

-- | An Index of `Interval`s.
data IntervalIndex (o :: Openness) = IntervalIndex
  {
  }

build :: forall n o a.
         Vec n (Interval o a)
      -> IntervalIndex o
build = panic "Interval.build"
  where
    _x_ = IntervalIndex
