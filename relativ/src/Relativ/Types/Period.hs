{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period
  ( Config(..)
  , ConfigValue(..)
  , Day
  , Hour
  , Minute
  , Month
  , Period
  , Second
  , Year
  , now
  ) where

import Relativ.Prelude

import Relativ.Types.Period.Day      ( Day )
import Relativ.Types.Period.Hour     ( Hour )
import Relativ.Types.Period.Minute   ( Minute )
import Relativ.Types.Period.Month    ( Month )
import Relativ.Types.Period.Second   ( Second )
import Relativ.Types.Period.Year     ( Year )
import Relativ.Types.Period.YearFreq ( YearFreq )

data Period = Period
  {
  } deriving (Eq, Ord, Show)

data Config = Config
  { value    :: ConfigValue
  -- , freq     :: _
  -- , oridinal :: _
  } deriving (Eq, Ord, Show)

data ConfigValue
  = StringValue Text
  | YearFreqValue YearFreq
  deriving (Eq, Ord, Show)

now :: MonadIO m => m Period
now = panic "Period.now"
