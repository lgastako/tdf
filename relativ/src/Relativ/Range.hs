{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.Range
  ( FromListError(..)
  , RangeIndex
  , fromList
  , fromList_
  , toList
  , toVec
  ) where

import Relativ.Prelude hiding ( toList )

import Relativ.Types.Name ( Name )

import qualified Data.Vec.Lazy      as Vec
import qualified Data.List.NonEmpty as NE

-- | Index implementing a monotonic integer range.
data RangeIndex a = RangeIndex
  { start :: a
  , stop  :: a
  , step  :: Int
  , name  :: Maybe Name
  } deriving (Eq, Ord, Show)

data FromListError
  = Unsequential
  deriving (Eq, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

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

-- ================================================================ --
--   Eliminators
-- ================================================================ --

-- | Convert the `RangeIndex` to a list of indexes.
toList :: forall a. Enum a => RangeIndex a -> [a]
toList RangeIndex {..} =
  [ start
  , toEnum (fromEnum start + step)
  , stop
  ]

-- | Conver the `RangeIndex` to a (Vec n) of indexes
toVec :: forall n a.
         ( Enum a
         , SNatI n
         )
      => RangeIndex a
      -> Maybe (Vec n a)
toVec = Vec.fromList . toList
