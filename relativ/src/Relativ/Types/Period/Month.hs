{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period.Month
  ( Month
    -- Constructors
  , fromInt
    -- Eliminators
  , toInt
  ) where

import Relativ.Prelude

-- TODO: Consider an enumration Jan | Feb | Mar ..
newtype Month = Month Int
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Month
fromInt n
  | n >= minMonth && n <= maxMonth = Just $ Month n
  | otherwise = Nothing
  where
    minMonth = 1
    maxMonth = 12

toInt :: Month -> Int
toInt (Month n) = n
