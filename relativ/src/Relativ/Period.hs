{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Period
  ( PeriodIndex
  , build
  ) where

import Relativ.Prelude

import Relativ.Types.Period ( Period )

-- | Index of `Period` data.
data PeriodIndex (n :: Nat) = PeriodIndex (Vec n Period)
  deriving (Eq, Ord, Show)

build :: Vec n Period -> PeriodIndex n
build = PeriodIndex
