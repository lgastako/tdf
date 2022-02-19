{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.Frame.Typed.Index.TimeDelta
  ( TimeDeltaIndex(..)
  ) where

import Data.Frame.Prelude

import Data.Frame.Typed.SubIndex ( SubIndex(..) )
import Data.Frame.Typed.ToVecN   ( ToVecN( toVecN ) )

data TimeDeltaIndex (n :: Nat) idx = TimeDeltaIndex
  deriving (Eq, Generic, Ord, Show)

instance SNatI n => SubIndex TimeDeltaIndex n idx where
  toLst = panic "Index.TimeDelta.toLst"
  drop  = panic "Index.TimeDelta.drop"
  take  = panic "Index.TimeDelta.take"

instance ToVecN (TimeDeltaIndex n idx) n idx where
  toVecN = panic "Index.TimeDelta.toVecN"
