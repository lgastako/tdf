{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.Frame.Typed.Index.Period
  ( PeriodIndex(..)
  ) where

import Data.Frame.Prelude

import Data.Frame.Typed.SubIndex ( SubIndex(..) )
import Data.Frame.Typed.ToVecN   ( ToVecN( toVecN ) )

data PeriodIndex (n :: Nat) idx = PeriodIndex
  deriving (Eq, Generic, Ord, Show)

instance SNatI n => SubIndex PeriodIndex n idx where
  toLst = panic "Index.Period.toLst"
  drop  = panic "Index.Period.drop"
  take  = panic "Index.Period.take"

instance ToVecN (PeriodIndex n idx) n idx where
  toVecN = panic "Index.Period.toVecN"
