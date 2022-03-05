{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.Range
  ( FromListError(..)
  , RangeIndex
  , fromList
  , fromList_
  , toList
  ) where

import Relativ.Prelude hiding ( toList )

import Relativ.Name ( Name )

import qualified Data.List.NonEmpty as NE

-- | Index implementing a monotonic integer range.
data RangeIndex a = RangeIndex
  { start :: a
  , stop  :: a
  , step  :: Int
  , name  :: Maybe Name
  } deriving (Eq, Ord, Show)

-- | Convert the `RangeIndex` to a list of indexes.
toList :: forall a. Enum a => RangeIndex a -> [a]
toList RangeIndex {..} =
  [ start
  , toEnum (fromEnum start + step)
  , stop
  ]

toVector :: RangeIndex a => Maybe (Vector n a)
toVector = undefined

data FromListError
  = Unsequential
  deriving (Eq, Ord, Show)

-- | Construct a `RangeIndex` from an optional name and an existing list.  The
-- existing list must be sequential.
fromList :: forall a.
            ( Enum a
            , Ord a
            )
         => Maybe Name
         -> NonEmpty a
         -> Either FromListError (RangeIndex a)
fromList name' xs
  | sequential xs = Right ri
  | otherwise     = Left Unsequential
  where
    ri = RangeIndex
      { start = start'
      , stop  = stop'
      , step  = step'
      , name  = name'
      }

    start', stop' :: a
    start' = NE.head xs
    stop'  = NE.last xs

    step' :: Int
    step' = case NE.tail xs of
      []    -> 1
      (x:_) -> fromEnum x - fromEnum start'

-- | Construct an unnamed `RangeIndex` from an an existing list.  The existing
-- list must be sequential.
fromList_ :: forall a.
             ( Enum a
             , Ord a
             )
          => NonEmpty a
          -> Either FromListError (RangeIndex a)
fromList_ = fromList Nothing
