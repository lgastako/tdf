{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Closedness
  ( Closedness(..)
  , FromProxy(..)
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

class FromProxy a where
  fromProxy :: a -> Closedness

instance FromProxy (Proxy 'Closed) where
  fromProxy _ = Closed

instance FromProxy (Proxy 'Open) where
  fromProxy _ = Open

instance FromProxy (Proxy 'ClosedLeft) where
  fromProxy _ = ClosedLeft

instance FromProxy (Proxy 'ClosedRight) where
  fromProxy _ = ClosedRight
