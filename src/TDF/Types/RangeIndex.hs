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

import           TDF.Prelude              hiding ( drop
                                                 , length
                                                 , take
                                                 , toList
                                                 )
import qualified TDF.Prelude     as P

import qualified Data.List       as List
import           TDF.Types.Index                 ( Index )
import qualified TDF.Types.Index as Index

data RangeIndex idx = RangeIndex
  { start :: idx
  , stop  :: idx
  , step  :: idx
  , name  :: Maybe Text
  } deriving (Eq, Generic, Ord, Read, Show)

instance (Bounded idx, Enum idx, Num idx) => Index idx (RangeIndex idx) where
  start   = start
  stop    = stop
  next ri = (+ step ri)

instance NFData idx => NFData (RangeIndex idx)

-- ================================================================ --
--  Constructors
-- ================================================================--

defaultFor :: ( Bounded idx
              , Enum idx
              , Foldable f
              )
           => f a
           -> RangeIndex idx
defaultFor = defaultFromFor minBound

defaultFromFor :: ( Bounded idx
                  , Enum idx
                  , Foldable f
                  )
               => idx
               -> f a
               -> RangeIndex idx
defaultFromFor start xs = RangeIndex
  { start = start
  , stop  = fromMaybe error
            . lastMay
            . zipWith const [toEnum 0 ..]
            . P.toList
            $ xs
  , step  = minBound
  , name  = Nothing
  }
  where
    error = panic "defaultFromFor.1"

stepping :: a -> a -> a -> RangeIndex a
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
length RangeIndex {..} = (fromEnum stop - fromEnum start) `div` fromEnum step

monotonicDecreasing :: (Integral a, Num a, Ord a) => RangeIndex a -> Bool
monotonicDecreasing ri@RangeIndex {..} = step < 0 || size ri <= 1

monotonicIncreasing :: (Integral a, Num a, Ord a) => RangeIndex a -> Bool
monotonicIncreasing ri@RangeIndex {..} = step > 0 || size ri <= 1

-- TODO: This is horrible (defeats the pointt), but fine for now
size :: (Integral a, Num a) => RangeIndex a -> Int
size = List.length . toList

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
