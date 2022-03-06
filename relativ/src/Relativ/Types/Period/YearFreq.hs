{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period.YearFreq
  ( YearFreq(..)
  , def
  ) where

import Relativ.Prelude hiding ( second )

import Relativ.Types.Period.Day       ( Day )
import Relativ.Types.Period.Frequency ( Frequency )
import Relativ.Types.Period.Hour      ( Hour )
import Relativ.Types.Period.Minute    ( Minute )
import Relativ.Types.Period.Month     ( Month )
import Relativ.Types.Period.Second    ( Second )
import Relativ.Types.Period.Year      ( Year )

import qualified Relativ.Types.Period.Frequency as Frequency
import qualified Relativ.Types.Period.Year      as Year

data YearFreq = YearFreq
  { year   :: Year
  , freq   :: Frequency
  , month  :: Maybe Month
  , day    :: Maybe Day
  , hour   :: Maybe Hour
  , minute :: Maybe Minute
  , second :: Maybe Second
  } deriving (Eq, Ord, Show)

def :: YearFreq
def = YearFreq
  { year   = Year.fromInt 1975
  , freq   = Frequency.A
  , month  = Nothing
  , day    = Nothing
  , hour   = Nothing
  , minute = Nothing
  , second = Nothing
  }
