{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Relativ.Prelude
  ( module X
  , (...)
  , sequential
  , subtractEnum
  , underEnum
  ) where

import Protolude as X

import Data.Vector.Sized as X ( Vector )

import qualified Data.List.NonEmpty as NE
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

-- | Returns true if the Foldable is empty, contains a single value, or
-- contains multiple values which are all the same distance apart in either
-- strictly ascending or descending order.
sequential :: forall f a.
              ( Enum a
              , Eq a
              , Foldable f
              , Ord a
              )
           => f a
           -> Bool
sequential xs
  | null xs   = True
  | otherwise = maybe True allSameDist . NE.nonEmpty . NE.tail $ xs'
  where
    xs'         = NE.fromList . toList $ xs
    allSameDist = (1 ==) . length . NE.nub . NE.zipWith subtractEnum xs'

subtractEnum :: forall a. Enum a => a -> a -> a
subtractEnum = underEnum (-)

underEnum :: forall a. Enum a => (Int -> Int -> Int) -> a -> a -> a
underEnum f (fromEnum -> a) (fromEnum -> b) = toEnum (f a b)
