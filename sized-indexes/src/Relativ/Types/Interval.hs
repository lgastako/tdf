{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.Types.Interval
  ( Interval
    -- Constructors
  , build
  , buildE
  , buildE_
  , build_
  , empty
    -- Combinators
  , in_
  ) where

import Relativ.Prelude hiding ( empty )

import Relativ.Types.Closedness ( Closedness( Open
                                            , Closed
                                            , ClosedLeft
                                            , ClosedRight
                                            )
                                )

import qualified Relativ.Types.Closedness as Closedness

data Interval a = Interval
  { closed :: Closedness
  , left   :: a
  , right  :: a
  } deriving (Eq, Ord, Show)

data BuildError
  = LeftNotLessThanOrEqualToRight
  deriving (Eq, Ord, Show)

instance Exception BuildError

-- ---------------------------------------------------------------- --
--   Constructors
-- ---------------------------------------------------------------- --

build :: forall a.
          Ord a
       => Closedness
       -> (a, a)
       -> Maybe (Interval a)
build = hush ... buildE

build_ :: forall a.
           Ord a
        => (a, a)
        -> Maybe (Interval a)
build_ = build Closedness.def

buildE :: forall a.
         Ord a
      => Closedness
      -> (a, a)
      -> Either BuildError (Interval a)
buildE clsd (l, r)
  | l <= r    = Right $ Interval clsd l r
  | otherwise = Left  $ LeftNotLessThanOrEqualToRight

buildE_ :: forall a.
           Ord a
        => (a, a)
        -> Either BuildError (Interval a)
buildE_ = buildE Closedness.def

-- ---------------------------------------------------------------- --
--   Combinators
-- ---------------------------------------------------------------- --

in_ :: forall a. Ord a => a -> Interval a -> Bool
in_ x Interval {..} = case closed of
  Open        -> left <  x && x <  right
  Closed      -> left <= x && x <= right
  ClosedLeft  -> left <= x && x <  right
  ClosedRight -> left <  x && x <= right

-- ---------------------------------------------------------------- --
--   Eliminators
-- ---------------------------------------------------------------- --

-- | Returns Just the answer when I know how to tell whether the interval is
--   empty or not and Nothing if I don't know.  If you are unsatisfied with the
--   coverage feel free to submit pull requests :)
empty :: Ord a => Interval a -> Maybe Bool
empty Interval {..}
  | left > right = Just True
  | otherwise    = Nothing
