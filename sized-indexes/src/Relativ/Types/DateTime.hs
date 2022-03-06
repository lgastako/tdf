{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Relativ.Types.DateTime
  ( DateTime
  , fromInt64
  , now
  , toInt64
  ) where

import Relativ.Prelude

newtype DateTime = DateTime Int64
  deriving (Eq, Ord, Show)

fromInt64 :: Int64 -> DateTime
fromInt64 = DateTime

now :: MonadIO m => m DateTime
now = panic "DateTime.now"

toInt64 :: DateTime -> Int64
toInt64 (DateTime t) = t
