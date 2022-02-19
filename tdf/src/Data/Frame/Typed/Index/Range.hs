{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Frame.Typed.Index.Range
  ( RangeIndex(..)
  -- Constructors
  , defaultFor
  , defaultFromFor
  -- Optics
  , name
  , start
  , step
  , stop
  -- Combinators
  , concat
  -- Eliminators
  , isMonotonicDecreasing
  , isMonotonicIncreasing
  , len
  ) where

import Data.Frame.Prelude hiding ( concat
                                 , toList
                                 )

import Data.Frame.Typed.SubIndex ( SubIndex(..) )
import Data.Frame.Typed.Name     ( Name )
import Data.Frame.Typed.ToVecN   ( ToVecN( toVecN ) )

import qualified Data.Frame.Typed.Name as Name
import qualified Data.Vec.Lazy.X       as Vec

-- TODO prevent zero or negative steps, ensure stop > start, and/or
--      support negative indexes properly too

-- | Immutable Index implementing a monotonic range over any Enum instance.
--
-- 'RangeIndex' is a memory-saving special case index limited to representing
-- monotonic ranges. Using 'RangeIndex' may in some instances improve computing
-- speed.
--
-- For consistency with the pandas version we store the 'stop' as one greater
-- than the max index, so when we generate actual collections of indexes we
-- need to stop one early.
--
-- This is the default index type used by 'Frame' and 'Series' when no explicit
-- index is provided by the user.
--
-- Since this is Haskell we don't need the dtype or copy properties from the
-- pandas version. We also don't need from range since ranges in Haskell are
-- just lists and there's 'fromLst' already.
--
-- We also don't do any of the sneaking around with treating start as stop if
-- it's an int and a full moon or mercury is in retrograde.  The types are what
-- they are.  Uses can easily create helpers for common uses cases specific to
-- their environments, or freel free to submit a pull-request if you think it
-- may be useful for others.

data RangeIndex (n :: Nat) idx = RangeIndex
  { riName  :: Maybe Name
  , riStart :: idx
  , riStep  :: Int
  , riStop  :: idx
  } deriving (Eq, Generic, Ord, Show)

-- Can't do it without a constraint (Num or Enum) on `a`.
-- instance SNatI n => Foldable (RangeIndex n)

instance forall n idx.
         ( Enum idx
         , SNatI n
         ) => SubIndex RangeIndex n idx where
  toLst ri = [start', next..stop']
    where
      start' = ri ^. start
      stop'  = pred (ri ^.stop)
      next   = ala Endo foldMap (replicate (ri ^. step) succ) $ ri ^. start

  drop = panic "Index.Range.drop"

  -- take :: ( SNatI n )
  --      => forall m.
  --         (LE m n)
  --      => RangeIndex n idx
  --      -> RangeIndex m idx
  take = panic "Index.Range.take"

instance ToVecN (RangeIndex n idx) n idx where
  toVecN = panic "Index.Range.toVecN"

-- ================================================================ --
--   Constructors
-- ================================================================ --

defaultFromFor :: forall n idx a.
                  ( Enum idx )
               => idx
               -> Vec n a
               -> RangeIndex n idx
defaultFromFor idx v = RangeIndex
  { riStart = idx
  , riStep  = 1
  , riStop  = toEnum $ fromEnum idx + Vec.length v
  , riName  = Nothing
  }

defaultFor :: forall n a. Vec n a -> RangeIndex n Int
defaultFor v = RangeIndex
  { riStart = toEnum 0
  , riStep  = 1
  , riStop  = toEnum $ Vec.length v
  , riName  = Nothing
  }

-- ================================================================ --
--   Optics
-- ================================================================ --

name :: Lens' (RangeIndex n idx) (Maybe Name)
name = field @"riName"

start :: Lens' (RangeIndex n idx) idx
start = field @"riStart"

step :: Lens' (RangeIndex n idx) Int
step = field @"riStep"

stop :: Lens' (RangeIndex n idx) idx
stop = field @"riStop"

-- ================================================================ --
--   Combinators
-- ================================================================ --

concat :: forall n m idx.
          ( Enum idx
          , Ord idx
          )
       => RangeIndex n idx
       -> RangeIndex m idx
       -> Maybe (RangeIndex (Plus m n) idx)
concat a b
  | riStep a /= riStep b = Nothing
  | otherwise = case stopMay of
      Nothing -> Nothing
      Just stop' -> Just $ RangeIndex
        { riStart = start'
        , riStep  = a ^. step
        , riStop  = stop'
        , riName  = Name.combine (a ^. name) (b ^. name)
        }
  where
    start' :: idx
    start' = a ^. start

    stopMay :: Maybe idx
    stopMay = case headMay (dropWhile (<= (a ^. stop)) [start'..]) of
      Nothing -> Nothing
      Just newStart -> Just . toEnum $ fromEnum newStart + deltaB

    -- TODO assumes ascending indexes which may not be correct?
    deltaB :: Int
    deltaB = fromEnum (b ^. stop) - fromEnum (b ^. start)

-- ================================================================ --
--   Eliminators
-- ================================================================ --

isMonotonicDecreasing :: Enum idx => RangeIndex n idx -> Bool
isMonotonicDecreasing idx = idx ^. step > 0 || len idx <= 1

isMonotonicIncreasing :: Enum idx => RangeIndex n idx -> Bool
isMonotonicIncreasing idx = idx ^. step < 0 || len idx <= 1

-- TODO move into SubIndex?
len :: Enum idx => RangeIndex n idx -> Int
len idx = fromEnum (idx ^. stop) - fromEnum (idx ^. start)
