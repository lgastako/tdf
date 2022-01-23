{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module TDF.Types.ListIndex
  ( ListIndex( ListIndex )
  , contains
  -- , monotonicDecreasing
  -- , monotonicIncreasing
  , size
  -- , stepping
  -- , through
  , toList
  -- , upTo
  ) where

import TDF.Prelude hiding ( toList )

newtype ListIndex a = ListIndex [a]
  deriving (Eq, Generic, Ord, Read, Show)

contains :: Ord a => ListIndex a -> a -> Bool
contains (ListIndex xs) n = n `elem` xs

-- monotonicDecreasing :: (Num a, Ord a) => ListIndex a -> Bool
-- monotonicDecreasing ri@(ListIndex xs) = step < 0 || size ri <= 1

-- monotonicIncreasing :: (Num a, Ord a) => ListIndex a -> Bool
-- monotonicIncreasing ri@ListIndex {..} = step > 0 || size ri <= 1

size :: (Bounded a, Enum a, Eq a, Num a) => ListIndex a -> a
size = \case
  ListIndex [] -> panic "ListIndex.size on empty index"
  ListIndex xs -> (fromMaybe error . lastMay) xs
                   -
                  (fromMaybe error . head) xs
  where
    error = panic "No last element in allegedly non-empty list."

-- stepping :: a -> a -> a -> ListIndex a
-- stepping a b c = ListIndex a b c Nothing

-- through :: Num a => a -> a -> ListIndex a
-- through a b = ListIndex
--   { start = a
--   , stop  = b
--   , step  = 1
--   , name  = Nothing
--   }

toList :: ListIndex idx -> [idx]
toList (ListIndex xs) = xs

-- upTo :: Num a => a -> ListIndex a
-- upTo = (0 `through`)

-- -- We do not implement things like `dtype` and `inferred_type` as they don't
-- -- make sense in Haskell where you always know the exact type at any given
-- -- time, and similarly we ignore things like `copy` that don't make sense for
-- -- other reasons (pervasive immutability, etc)
