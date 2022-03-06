{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period.Minute
  ( Minute
    -- Constructors
  , fromInt
    -- Eliminators
  , toInt
  ) where

import Relativ.Prelude

newtype Minute = Minute Int
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Minute
fromInt n
  | n >= minMinute && n <= maxMinute = Just $ Minute n
  | otherwise = Nothing
  where
    minMinute = 0
    maxMinute = 59

toInt :: Minute -> Int
toInt (Minute n) = n
