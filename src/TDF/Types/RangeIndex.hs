{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}

module TDF.Types.RangeIndex
  ( RangeIndex( RangeIndex )
  , contains
  , monotonicDecreasing
  , monotonicIncreasing
  , size
  , stepping
  , take
  , through
  , toList
  , upTo
  ) where

import           TDF.Prelude              hiding ( take
                                                 , toList
                                                 )

import qualified Data.List       as List
import           TDF.Types.Index                 ( Index )
import qualified TDF.Types.Index as Index

data RangeIndex a = RangeIndex
  { start :: a
  , stop  :: a
  , step  :: a
  , name  :: Maybe Text
  } deriving (Eq, Generic, Ord, Read, Show)

instance (Bounded idx, Enum idx, Num idx) => Index idx (RangeIndex idx) where
  start   = start
  stop    = stop
  next ri = (+ step ri)

-- ================================================================ --
--  Constructors
-- ================================================================--

stepping :: a -> a -> a -> RangeIndex a
stepping a b c = RangeIndex a b c Nothing

through :: Num a => a -> a -> RangeIndex a
through a b = RangeIndex
  { start = a
  , stop  = b
  , step  = 1
  , name  = Nothing
  }

upTo :: Num a => a -> RangeIndex a
upTo = (0 `through`)

-- ================================================================ --
--   Combinators
-- ================================================================ --

take :: forall idx.
        ( Enum idx
        , Eq idx
        , Num idx
        )
     => Int
     -> RangeIndex idx
     -> RangeIndex idx
take n ri@RangeIndex {..} = RangeIndex
  { start = start
  , stop  = stop'
  , step  = step
  , name  = name
  }
  where
    zipped :: [idx]
    zipped = List.take n . toList $ ri

    stop' = case lastMay zipped of
      Just m  -> m
      Nothing -> panic "explodypants"

-- ================================================================ --
--  Eliminators
-- ================================================================ --

contains :: Ord a => RangeIndex a -> a -> Bool
contains RangeIndex {..} n  = n >= start && n < stop

monotonicDecreasing :: (Num a, Ord a) => RangeIndex a -> Bool
monotonicDecreasing ri@RangeIndex {..} = step < 0 || size ri <= 1

monotonicIncreasing :: (Num a, Ord a) => RangeIndex a -> Bool
monotonicIncreasing ri@RangeIndex {..} = step > 0 || size ri <= 1

size :: Num a => RangeIndex a -> a
size RangeIndex {..} = stop - start

toList :: (Enum a, Eq a, Num a) => RangeIndex a -> [a]
toList RangeIndex {..} = [start, start + step .. stop + offset]
  where
    offset | signum step == -1 =  1
           | otherwise         = -1

-- TODO: Document this better - headerdoc?

-- We do not implement things like `dtype` and `inferred_type` as they don't
-- make sense in Haskell where you always know the exact type at any given
-- time, and similarly we ignore things like `copy` that don't make sense for
-- other reasons (pervasive immutability, etc)
