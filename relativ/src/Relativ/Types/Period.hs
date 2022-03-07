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
  , fromText
  , fromFreqText
  , fromFreqTime
  , now
  ) where

import Relativ.Prelude

import Relativ.Types.Period.Day       ( Day )
import Relativ.Types.Period.Frequency ( Frequency )
import Relativ.Types.Period.Hour      ( Hour )
import Relativ.Types.Period.Minute    ( Minute )
import Relativ.Types.Period.Month     ( Month )
import Relativ.Types.Period.Second    ( Second )
import Relativ.Types.Period.Year      ( Year )
import Relativ.Types.Period.YearFreq  ( YearFreq )

import qualified Relativ.Types.Period.Frequency as Frequency

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

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromText :: Text -> Maybe Period
fromText = fromFreqText Frequency.def

fromFreqText :: Frequency -> Text -> Maybe Period
fromFreqText _freq _s = Just Period

fromFreqTime :: Frequency -> UTCTime -> Period
fromFreqTime _freq _t = Period

now :: MonadIO m => Frequency -> m Period
now freq = fromFreqTime freq <$> liftIO getCurrentTime
