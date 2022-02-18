{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
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
  , defaultFromFor
  , defaultIntsFor
  -- Optics
  , name
  , start
  , step
  , stop
  -- Combinators
  , append
  ) where

import Data.Frame.Prelude     hiding ( toList )

import Data.Frame.Typed.SubIndex     ( SubIndex(..) )
import Data.Frame.Typed.Types.Name   ( Name )
import Data.Frame.Typed.Types.ToVecN ( ToVecN( toVecN ) )

import qualified Data.Frame.Typed.Types.Name as Name
import qualified Data.Vec.Lazy.X             as Vec

-- TODO prevent zero or negative steps, ensure stop > start, and/or
--      support negative indexes properly too

-- | Immutable Index implementing a monotonic range over any Enum instance.
--
-- RangeIndex is a memory-saving special case index limited to representing
-- monotonic ranges. Using RangeIndex may in some instances improve computing
-- speed.
--
-- For consistency with the pandas version with store the stop as one greater
-- than the max index, so when we generate actual collections of indexes we
-- need to stop one early.
data RangeIndex (n :: Nat) idx = RangeIndex
  { riName  :: Maybe Name
  , riStart :: idx
  , riStep  :: Int
  , riStop  :: idx
  } deriving (Eq, Generic, Ord, Show)

-- Can't do it without a constraint (Num or Enum) on `a`.
-- instance SNatI n => Foldable (RangeIndex n)

instance ( Enum idx
         , SNatI n
         ) => SubIndex RangeIndex n idx where
  toLst RangeIndex {..} = [riStart, next..pred riStop]
    where
      next = ala Endo foldMap (replicate riStep succ) riStart

  drop = panic "Index.Range.drop"
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

defaultIntsFor :: forall n a. Vec n a -> RangeIndex n Int
defaultIntsFor v = RangeIndex
  { riStart = 0
  , riStep  = 1
  , riStop  = Vec.length v
  , riName  = Nothing
  }

-- ================================================================ --
--   Optics
-- ================================================================ --

start :: Lens' (RangeIndex n idx) idx
start = field @"riStart"

step :: Lens' (RangeIndex n idx) Int
step = field @"riStep"

stop :: Lens' (RangeIndex n idx) idx
stop = field @"riStop"

name :: Lens' (RangeIndex n idx) (Maybe Name)
name = field @"riName"

-- ================================================================ --
--   Combinators
-- ================================================================ --

append :: forall n m idx.
          ( Enum idx
          , Ord idx
          )
       => RangeIndex n idx
       -> RangeIndex m idx
       -> Maybe (RangeIndex (Plus m n) idx)
append a b
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
