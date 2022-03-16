{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.Series
  ( Series
    -- Constructors
  , fromVector
    -- Optics
  , vector
    -- Eliminators
  , toVector
  ) where

import Relativ.Prelude

import qualified Data.Vector.Sized as S

newtype Series (n :: Nat) a = Series (S.Vector n a)
  deriving (Eq, Functor, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromVector :: S.Vector n a -> Series n a
fromVector = Series

-- ================================================================ --
--   Optics
-- ================================================================ --

vector :: forall n m a b.
          Lens (Series n a)   (Series m b)
               (S.Vector n a) (S.Vector m b)
vector = lens toVector (const fromVector)

-- ================================================================ --
--   Combinators
-- ================================================================ --

-- ================================================================ --
--   Eliminators
-- ================================================================ --

toVector :: Series n a -> S.Vector n a
toVector (Series v) = v
