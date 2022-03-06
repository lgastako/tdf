{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period.Year
  ( Year
    -- Constructors
  , fromInt
    -- Eliminators
  , toInt
  ) where

import Relativ.Prelude

newtype Year = Year Int
  deriving (Eq, Ord, Show)

fromInt :: Int -> Year
fromInt = Year

toInt :: Year -> Int
toInt (Year y) = y
