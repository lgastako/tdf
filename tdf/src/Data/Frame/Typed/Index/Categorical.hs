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

import Data.Frame.Typed.SubIndex     ( SubIndex(..) )
import Data.Frame.Typed.Types.ToVecN ( ToVecN( toVecN ) )

newtype CategoricalIndex (n :: Nat) idx = CategoricalIndex (Vec n idx)
  deriving (Eq, Generic, Ord, Show)

instance SNatI n => SubIndex CategoricalIndex n idx where
  toLst = panic "Index.Categorical.toLst"
  drop  = panic "Index.Categorical.drop"
  take  = panic "Index.Categorical.take"

instance ToVecN (CategoricalIndex n idx) n idx where
  toVecN = panic "Index.Categorical.toVecN"

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromVec :: Vec n idx -> CategoricalIndex n idx
fromVec = CategoricalIndex
