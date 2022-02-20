{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid.Index.Range
  ( RangeIndex
  -- Constructors
  , empty
  , fromTo
  , fromToBy
  ) where

import Data.Grid.Prelude hiding ( empty )

newtype RangeIndex (n :: Nat) ix = RangeIndex (Int, ix, ix)
  deriving (Eq, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: Enum ix => RangeIndex 0 ix
empty = fromTo (toEnum 0) (toEnum 0)

fromTo :: forall n ix. ix -> ix -> RangeIndex n ix
fromTo = fromToBy 1

fromToBy :: forall n ix. Int -> ix -> ix -> RangeIndex n ix
fromToBy step start stop = RangeIndex (step, start, stop)
