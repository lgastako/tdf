{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period.Day
  ( Day
    -- Constructors
  , fromInt
    -- Eliminators
  , toInt
  ) where

import Relativ.Prelude

newtype Day = Day Int
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Day
fromInt n
  | n >= minDay && n <= maxDay = Just $ Day n
  | otherwise = Nothing
  where
    minDay = 1
    maxDay = 31

toInt :: Day -> Int
toInt (Day n) = n
