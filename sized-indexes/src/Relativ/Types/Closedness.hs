{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Closedness
  ( Closedness(..)
  , def
  ) where

import Relativ.Prelude

data Closedness
  = Open
  | Closed
  | ClosedLeft
  | ClosedRight
  deriving (Eq, Ord, Show)

def :: Closedness
def = ClosedRight
