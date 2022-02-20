{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Grid.Index.Vector
  ( VectorIndex
  -- Constructors
  , fromVector
  ) where

import Data.Grid.Prelude

import qualified Data.Vector.Sized as Sized

newtype VectorIndex (n :: Nat) ix = VectorIndex (Sized.Vector n ix)
  deriving (Eq, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromVector :: Sized.Vector n ix -> VectorIndex n ix
fromVector = VectorIndex
