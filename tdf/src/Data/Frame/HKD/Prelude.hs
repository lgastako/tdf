{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Frame.HKD.Prelude
  ( module X
  , HKD
  ) where

import Data.Frame.Prelude as X -- TODO hiding

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a
