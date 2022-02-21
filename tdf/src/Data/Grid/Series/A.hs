{-# LANGUAGE NoImplicitPrelude #-}

module Data.Grid.Series.A
  ( ASeries
  ) where

import Data.Grid.Prelude
import Data.Grid.Series ( Series )

data ASeries k a = forall n. KnownNat n => ASeries (Series n k a)
