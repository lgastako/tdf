{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Period
  ( PeriodIndex
  , build
  ) where

import Relativ.Prelude

import Relativ.Types.Period ( Period )

import qualified Data.Vector.Sized as S

-- | Index of `Period` data.
data PeriodIndex (n :: Nat) = PeriodIndex (S.Vector n Period)
  deriving (Eq, Ord, Show)

build :: S.Vector n Period -> PeriodIndex n
build = PeriodIndex
