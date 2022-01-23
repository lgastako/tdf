{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module TDF.Types.RangeIndex
  ( RangeIndex( RangeIndex )
  , contains
  , defaultFor
  , defaultFromFor
  , drop
  , empty
  , length
  , monotonicDecreasing
  , monotonicIncreasing
  , size
  , stepping
  , take
  , through
  , toList
  , upTo
  ) where

import           TDF.Prelude         hiding ( drop
                                            , empty
                                            , length
                                            , one
                                            , take
                                            , toList
                                            )

import qualified Data.List          as List
import           TDF.Types.Positive         ( Positive
                                            , one
                                            , unPositive
                                            )

data RangeIndex idx = RangeIndex
  { start :: idx
  , stop  :: idx
  , step  :: Positive Int
  , name  :: Maybe Text
  } deriving (Eq, Generic, Ord, Show)

instance NFData idx => NFData (RangeIndex idx)

-- ================================================================ --
--  Constructors
-- ================================================================--

-- TODO explode on zero steps?

empty :: RangeIndex Int
empty = RangeIndex 0 0 one Nothing

defaultFor :: Foldable f
           => f a
           -> RangeIndex Int
defaultFor = defaultFromFor 0

defaultFromFor :: Foldable f
               => Int
               -> f a
               -> RangeIndex Int
defaultFromFor start xs = RangeIndex
  { start = start
  , stop  = List.length xs
  , step  = one
  , name  = Nothing
  }

stepping :: a -> a -> Positive Int -> RangeIndex a
stepping a b c = RangeIndex a b c Nothing

through :: (Num a, Ord a) => a -> a -> RangeIndex a
through a b = RangeIndex
  { start = a
  , stop  = if b > a then b else a
  , step  = 1
  , name  = Nothing
  }

upTo :: (Num a, Ord a) => a -> RangeIndex a
upTo = (0 `through`)

-- ================================================================ --
--   Combinators
-- ================================================================ --

drop :: forall idx.
        ( Enum idx
        , Eq idx
        , Num idx
        )
     => Int
     -> RangeIndex idx
     -> RangeIndex idx
drop n ri@RangeIndex {..} = RangeIndex
  { start = start
  , stop  = stop'
  , step  = step
  , name  = name
  }
  where
    zipped :: [idx]
    zipped = List.drop n . toList $ ri

    stop' = case lastMay zipped of
      Just m  -> m
      Nothing -> panic "explodypants"

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

length :: Enum idx => RangeIndex idx -> Int
length _ri@RangeIndex {..}
  | step < 0   = panic "RI.length (neg)"
  | otherwise = (fromEnum stop - fromEnum start) `div` fromEnum step

monotonicDecreasing :: (Integral a, Num a, Ord a) => RangeIndex a -> Bool
monotonicDecreasing ri@RangeIndex {..} = step < 0 || size ri <= 1

monotonicIncreasing :: (Integral a, Num a, Ord a) => RangeIndex a -> Bool
monotonicIncreasing ri@RangeIndex {..} = step > 0 || size ri <= 1

-- TODO: This is horrible (defeats the pointt), but fine for now
size :: (Integral a, Num a) => RangeIndex a -> Int
size = List.length . toList

toList :: (Enum a, Eq a, Num a) => RangeIndex a -> [a]
toList RangeIndex {..} = [start, start + step'' .. stop + offset]
  where
    offset | signum step == -1 =  1
           | otherwise         = -1

    step'  = unPositive step
    step'' = toEnum . (+ step') . fromEnum $ step

-- TODO: Document this better - headerdoc?

-- We do not implement things like `dtype` and `inferred_type` as they don't
-- make sense in Haskell where you always know the exact type at any given
-- time, and similarly we ignore things like `copy` that don't make sense for
-- other reasons (pervasive immutability, etc)
