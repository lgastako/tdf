{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module TDF.Types.RangeIndex
  ( RangeIndex( RangeIndex )
  , contains
  , monotonicDecreasing
  , monotonicIncreasing
  , size
  , stepping
  , through
  , toList
  , upTo
  ) where

import TDF.Prelude hiding ( toList )

data RangeIndex a = RangeIndex
  { start :: a
  , stop  :: a
  , step  :: a
  , name  :: Maybe Text
  } deriving (Eq, Generic, Ord, Read, Show)

contains :: Ord a => RangeIndex a -> a -> Bool
contains RangeIndex {..} n  = n >= start && n < stop

monotonicDecreasing :: (Num a, Ord a) => RangeIndex a -> Bool
monotonicDecreasing ri@RangeIndex {..} = step < 0 || size ri <= 1

monotonicIncreasing :: (Num a, Ord a) => RangeIndex a -> Bool
monotonicIncreasing ri@RangeIndex {..} = step > 0 || size ri <= 1

size :: Num a => RangeIndex a -> a
size RangeIndex {..} = stop - start

stepping :: a -> a -> a -> RangeIndex a
stepping a b c = RangeIndex a b c Nothing

through :: Num a => a -> a -> RangeIndex a
through a b = RangeIndex
  { start = a
  , stop  = b
  , step  = 1
  , name  = Nothing
  }

toList :: (Enum a, Eq a, Num a) => RangeIndex a -> [a]
toList RangeIndex {..} = [start, start + step .. stop + offset]
  where
    offset | signum step == -1 = 1
           | otherwise         = -1

upTo :: Num a => a -> RangeIndex a
upTo = (0 `through`)

-- We do not implement things like `dtype` and `inferred_type` as they don't
-- make sense in Haskell where you always know the exact type at any given
-- time, and similarly we ignore things like `copy` that don't make sense for
-- other reasons (pervasive immutability, etc)
