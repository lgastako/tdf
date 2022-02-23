{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Tensor
  ( Tensor
  -- Constructors
  , fromMap
  -- Eliminators
  , toMap
  ) where

import Protolude

data Tensor ixs a = Tensor (Map ixs a)
  deriving (Eq, Generic, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromMap :: Map ixs a -> Tensor ixs a
fromMap = Tensor

-- ================================================================ --
--   Eliminators
-- ================================================================ --

toMap :: Tensor ixs a -> Map ixs a
toMap (Tensor m) = m
