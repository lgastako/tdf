{-# LANGUAGE NoImplicitPrelude #-}

module Data.Vec.Lazy.X
  ( module Data.Vec.Lazy
  , find
  ) where

import Prelude hiding (foldr)

import Data.Vec.Lazy

find :: (a -> Bool) -> Vec n a -> Maybe a
find p = foldr f Nothing
  where
    f _ found@(Just _) = found
    f x Nothing | p x = Just x
                | otherwise = Nothing
