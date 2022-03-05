{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Relativ.Flag
  ( Flag
  , toFlag
  , fromFlag
  ) where

import Relativ.Prelude

newtype Flag a = Flag Bool
  deriving (Eq, Ord, Show)

fromFlag :: a -> Flag a -> Bool
fromFlag _ (Flag x) = x

toFlag :: a -> Bool -> Flag a
toFlag _ = Flag
