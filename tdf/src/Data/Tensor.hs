{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Tensor
  ( Tensor
  -- Constructors
  , empty
  , fromMap
  , pure
  -- Combinators
  , ( *>)
  , (<$ )
  , ( $>)
  , (<&>)
  , (<$>)
  , (<* )
  , (<**>)
  , (<*>)
  , fmap
  , liftA2
  , liftA3
  , liftM2
  , liftM3
  , liftM4
  , liftM5
    -- Eliminators
  , all
  , and
  , any
  , asum
  , concat
  , concatMap
  , elem
  , find
  , fold
  , foldMap
  , foldl'
  , foldr
  , foldrM
  , for_
  , forM_
  , maximumBy
  , minimumBy
  , msum
  , notElem
  , null
  , or
  , product
  , sequence
  , sequence_
  , sequenceA
  , sequenceA_
  , sum
  , toList
  , toMap
  , traverse
  , traverse_
  , void
  ) where

import Protolude hiding ( empty )

import qualified Data.Map.Strict as Map

data Tensor ixs a = Tensor (Map ixs a)
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Applicative (Tensor Int) where
  pure  x = Tensor (Map.singleton 0 x)
  _f <*> _x = result
    where
      -- _ = f :: Tensor Int (a -> b)
      -- _ = x :: Tensor Int a

      result :: Tensor Int b
      result = panic "Tensor.result"

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: Tensor idx a
empty = Tensor Map.empty

fromMap :: Map ixs a -> Tensor ixs a
fromMap = Tensor

-- ================================================================ --
--   Eliminators
-- ================================================================ --

toMap :: Tensor ixs a -> Map ixs a
toMap (Tensor m) = m
