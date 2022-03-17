{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.Series.Unsized
  ( Series
    -- Constructors
  , fromVector
    -- Optics
  , vector
    -- Eliminators
  , toVector
  ) where

import Relativ.Prelude

import qualified Data.Vector as U

newtype Series a = Series (U.Vector a)
  deriving (Eq, Functor, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromVector :: U.Vector a -> Series a
fromVector = Series

-- ================================================================ --
--   Optics
-- ================================================================ --

vector :: forall a b.
          Lens (Series   a) (Series   b)
               (U.Vector a) (U.Vector b)
vector = lens toVector (const fromVector)

-- ================================================================ --
--   Combinators
-- ================================================================ --

class Locator loc a where
  loc :: loc -> a -> a

instance Locator (Int, Int) (U.Vector a) where
  loc = undefined

-- loc :: Locator loc
--     => loc
--     -> Series n a
--     -> Series m a
-- loc = undefined

-- class Locator loc a | a -> loc where
--   loc :: loc -> a -> b

-- ================================================================ --
--   Eliminators
-- ================================================================ --

toVector :: Series a -> U.Vector a
toVector (Series v) = v
