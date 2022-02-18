{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.Frame.Typed.Index.Interval
  ( IntervalIndex(..)
  ) where

import Data.Frame.Prelude

import Data.Frame.Typed.SubIndex     ( SubIndex(..) )
import Data.Frame.Typed.Types.ToVecN ( ToVecN( toVecN ) )

data IntervalIndex (n :: Nat) idx = IntervalIndex
  deriving (Eq, Generic, Ord, Show)

instance SNatI n => SubIndex IntervalIndex n idx where
  toLst = panic "Index.Interval.toLst"
  drop  = panic "Index.Interval.drop"
  take  = panic "Index.Interval.take"

instance ToVecN (IntervalIndex n idx) n idx where
  toVecN = panic "Index.Interval.toVecN"
