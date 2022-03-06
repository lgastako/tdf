{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Types.Openness
  ( Openness(..)
  , FromProxy(..)
  , HasOpenness
  , def
  ) where

import Relativ.Prelude

data Openness
  = Open
  | Closed
  | ClosedLeft
  | ClosedRight
  deriving (Eq, Ord, Show)

type HasOpenness (c :: Openness) = FromProxy (Proxy c)

def :: Openness
def = ClosedRight

class FromProxy a where
  fromProxy :: a -> Openness

instance FromProxy (Proxy 'Closed) where
  fromProxy _ = Closed

instance FromProxy (Proxy 'Open) where
  fromProxy _ = Open

instance FromProxy (Proxy 'ClosedLeft) where
  fromProxy _ = ClosedLeft

instance FromProxy (Proxy 'ClosedRight) where
  fromProxy _ = ClosedRight
