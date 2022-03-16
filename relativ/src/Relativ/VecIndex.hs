{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.VecIndex
  ( VecIndex
  , fromList
  , fromVec
  ) where

import Relativ.Prelude

import qualified Data.Vec.Lazy as Vec

-- | Immutable sequence used for indexing and alignment. The basic object
-- storing axis labels for all TDF objects.
newtype VecIndex n a = VecIndex (Vec n a)
  deriving (Eq, Ord, Show)

fromList :: forall n a. SNatI n => [a] -> Maybe (VecIndex n a)
fromList = fmap fromVec . Vec.fromList

fromVec :: forall n a. Vec n a -> VecIndex n a
fromVec = VecIndex
