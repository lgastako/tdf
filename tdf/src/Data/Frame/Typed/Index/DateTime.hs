{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Frame.Typed.Index.DateTime
  ( DateTimeIndex(..)
  ) where

import Data.Frame.Prelude hiding ( toList )

import Data.Frame.Typed.SubIndex ( SubIndex(..) )
import Data.Frame.Typed.ToVecN   ( ToVecN( toVecN ) )

data DateTimeIndex (n :: Nat) idx = DateTimeIndex
  deriving (Eq, Generic, Ord, Show)

instance SNatI n => SubIndex DateTimeIndex n idx where
  toLst = panic "Index.DateTime.toLst"
  drop  = panic "Index.DateTime.drop"
  take  = panic "Index.DateTime.take"

instance ToVecN (DateTimeIndex n idx) n idx where
  toVecN = panic "Index.DateTime.toVecN"
