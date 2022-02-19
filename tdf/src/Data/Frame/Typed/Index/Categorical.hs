{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.Frame.Typed.Index.Categorical
  ( CategoricalIndex(..)
  -- Constructors
  , fromVec
  ) where

import Data.Frame.Prelude

import Data.Frame.Typed.SubIndex ( SubIndex(..) )
import Data.Frame.Typed.ToVecN   ( ToVecN( toVecN ) )

import qualified Data.Frame.Typed.SubIndex as SubIndex

data CategoricalIndex (n :: Nat) idx = CategoricalIndex
  { toVec :: Vec n idx }
  deriving (Eq, Generic, Ord, Show)

instance SNatI n => SubIndex CategoricalIndex n idx where
  toLst = toList . SubIndex.toVec

  drop  = panic "Index.Categorical.drop"
  take  = panic "Index.Categorical.take"

instance SNatI n => ToVecN (CategoricalIndex n idx) n idx where
  toVecN = SubIndex.toVec

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromVec :: Vec n idx -> CategoricalIndex n idx
fromVec = CategoricalIndex
