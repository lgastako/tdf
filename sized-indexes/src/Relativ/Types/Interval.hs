{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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

import Relativ.Types.Closedness ( Closedness( Open
                                            , Closed
                                            , ClosedLeft
                                            , ClosedRight
                                            )
                                , FromProxy
                                )

import qualified Relativ.Types.Closedness  as Closedness

newtype Interval (c :: Closedness) a = Interval (a, a)
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------- --
--   Constructors
-- ---------------------------------------------------------------- --

build :: forall a c.
         Ord a
      => (a, a)
      -> Interval c a
build = Interval

-- ---------------------------------------------------------------- --
--   Eliminators
-- ---------------------------------------------------------------- --

-- | Returns Just the answer when I know how to tell whether the interval is
--   empty or not and Nothing if I don't know.  If you are unsatisfied with the
--   coverage feel free to submit pull requests :)
empty :: forall a c. Ord a => Interval c a -> Maybe Bool
empty (Interval (left, right))
  | left > right = Just True
  | otherwise    = Nothing

member :: forall a c.
          ( FromProxy (Proxy c)
          , Ord a
          )
       => a
       -> Interval c a
       -> Bool
member x (Interval (left, right)) = case closed of
  Open        -> left <  x && x <  right
  Closed      -> left <= x && x <= right
  ClosedLeft  -> left <= x && x <  right
  ClosedRight -> left <  x && x <= right
  where
    closed :: Closedness
    closed = Closedness.fromProxy (Proxy @c)

-- The other interval libraries do it. Maybe their implementations are
-- optimized.  But we are going for DX, so sounds good to me.
notMember :: forall a c.
             ( FromProxy (Proxy c)
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
