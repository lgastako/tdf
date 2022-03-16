{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.VecIndex
  ( VecIndex
  , fromList
  , fromVec
  ) where

import Relativ.Prelude

import qualified Data.Vector.Sized as S

-- | Immutable sequence used for indexing and alignment. The basic object
-- storing axis labels for all TDF objects.
newtype VecIndex n a = VecIndex (S.Vector n a)
  deriving (Eq, Ord, Show)

fromList :: forall n a. KnownNat n => [a] -> Maybe (VecIndex n a)
fromList = fmap fromVec . S.fromList

fromVec :: forall n a. S.Vector n a -> VecIndex n a
fromVec = VecIndex
