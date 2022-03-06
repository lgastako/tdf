{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period.Hour
  ( Hour
    -- Constructors
  , fromInt
    -- Eliminators
  , toInt
  ) where

import Relativ.Prelude

newtype Hour = Hour Int
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Hour
fromInt n
  | n >= minHour && n <= maxHour = Just $ Hour n
  | otherwise = Nothing
  where
    minHour = 0
    maxHour = 23

toInt :: Hour -> Int
toInt (Hour n) = n
