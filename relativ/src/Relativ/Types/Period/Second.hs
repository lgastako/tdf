{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period.Second
  ( Second
    -- Constructors
  , fromInt
    -- Eliminators
  , toInt
  ) where

import Relativ.Prelude

newtype Second = Second Int
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Second
fromInt n
  | n >= minSecond && n <= maxSecond = Just $ Second n
  | otherwise = Nothing
  where
    minSecond = 0
    maxSecond = 59

toInt :: Second -> Int
toInt (Second n) = n
