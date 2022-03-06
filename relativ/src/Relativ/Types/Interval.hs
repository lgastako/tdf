{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Relativ.Types.Interval
  ( Interval
    -- Constructors
  , build
    -- Eliminators
  , empty
  , member
  , notMember
  , overlaps
  ) where

import Relativ.Prelude hiding ( empty )

import Relativ.Types.Openness ( HasOpenness
                              , Openness( Open
                                        , Closed
                                        , ClosedLeft
                                        , ClosedRight
                                        )
                              )

import qualified Relativ.Types.Openness  as Openness

newtype Interval (c :: Openness) a = Interval (a, a)
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------- --
--   Constructors
-- ---------------------------------------------------------------- --

build :: forall o a.
         ( Ord a )
      => (a, a)
      -> Interval o a
build = Interval

-- ---------------------------------------------------------------- --
--   Eliminators
-- ---------------------------------------------------------------- --

-- | Returns Just the answer when I know how to tell whether the interval is
--   empty or not and Nothing if I don't know.  If you are unsatisfied with the
--   coverage feel free to submit pull requests :)
empty :: forall a o.
         ( Ord a )
      => Interval o a
      -> Maybe Bool
empty (Interval (left, right))
  | left > right = Just True
  | otherwise    = Nothing

member :: forall a c.
          ( HasOpenness c
          , Ord a
          )
       => a
       -> Interval c a
       -> Bool
member x (Interval (left, right)) = case Openness.fromProxy (Proxy @c) of
  Open        -> left <  x && x <  right
  Closed      -> left <= x && x <= right
  ClosedLeft  -> left <= x && x <  right
  ClosedRight -> left <  x && x <= right

-- The other interval libraries do it. Maybe their implementations are
-- optimized.  But we are going for DX, so sounds good to me.
notMember :: forall a c.
             ( HasOpenness c
             , Ord a
             )
          => a
          -> Interval c a
          -> Bool
notMember = not ... member

overlaps :: forall c c' a.
            Interval c a
         -> Interval c' a
         -> Bool
overlaps _a _b = panic "Interval.overlaps"
