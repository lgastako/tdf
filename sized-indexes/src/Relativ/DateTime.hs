{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Relativ.DateTime
  ( DateTimeIndex
  , build
  ) where

import Relativ.Prelude

import Relativ.Flag

-- | Index of `datetime64` data.
data DateTimeIndex = DateTimeIndex
  deriving (Eq, Ord, Show)

data Normalize = Normalize
  deriving (Eq, Ord, Show)

data FieldOrder
  = DayFirst
  | YearFirst
  deriving (Eq, Ord, Show)

build :: Flag Normalize -> ()
build = panic "DateTime.build"
