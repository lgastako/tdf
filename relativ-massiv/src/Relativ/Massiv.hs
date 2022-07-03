{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.Massiv
  ( Series
  , fromList
  ) where

import Relativ.Prelude

import qualified Data.Massiv.Array as A

newtype Series r a = Series (A.Vector r a)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromList :: forall r a.
            A.Mutable r A.Ix1 a
         => A.Comp
         -> [a]
         -> Series r a
fromList c = Series . A.fromList c

-- ================================================================ --
--   Optics
-- ================================================================ --

-- ================================================================ --
--   Combinators
-- ================================================================ --

-- ================================================================ --
--   Eliminators
-- ================================================================ --
