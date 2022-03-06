{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Period.Frequency
  ( Frequency(..)
  , description
  , min
  , ms
  , us
  ) where

import Relativ.Prelude hiding ( min )

-- Pandas search term "offset aliases"
-- https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html#offset-aliases

data Frequency
  = A
  | AS
  | B
  | BA
  | BAS
  | BH
  | BM
  | BMS
  | BQ
  | BQS
  | BY
  | BYS
  | C
  | CBM
  | CBMS
  | D
  | H
  | L
  | M
  | MS
  | N
  | Q
  | QS
  | S
  | SM
  | SMS
  | T
  | U
  | W
  | Y
  | YS
  deriving (Eq, Ord, Show)

description :: Frequency -> Text
description = \case
  A    -> "year end frequency"
  AS   -> "year start frequency"
  B    -> "business day frequency"
  BA   -> "business year end frequency"
  BAS  -> "business year start frequency"
  BH   -> "business hour frequency"
  BM   -> "business month end frequency"
  BMS  -> "business month start frequency"
  BQ   -> "business quarter end frequency"
  BQS  -> "business quarter start frequency"
  BY   -> "business year end frequency"
  BYS  -> "business year start frequency"
  C    -> "custom business day frequency"
  CBM  -> "custom business month end frequency"
  CBMS -> "custom business month start frequency"
  D    -> "calendar day frequency"
  H    -> "hourly frequency"
  L    -> "milliseconds"
  M    -> "month end frequency"
  MS   -> "month start frequency"
  N    -> "nanoseconds"
  Q    -> "quarter end frequency"
  QS   -> "quarter start frequency"
  S    -> "secondly frequency"
  SM   -> "semi-month end frequency (15th and end of month)"
  SMS  -> "semi-month start frequency (1st and 15th)"
  T    -> "minutely frequency"
  U    -> "microseconds"
  W    -> "weekly frequency"
  Y    -> "year end frequency"
  YS   -> "year start frequency"

min :: Frequency
min = T

ms :: Frequency
ms = L

us :: Frequency
us = U
