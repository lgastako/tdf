{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Orphans () where

import Relativ.Prelude

import Test.QuickCheck

import Relativ.Types.Period.Year ( Year )

import qualified Relativ.Types.Period.Year as Year

instance Arbitrary Year where
  arbitrary = Year.fromInt <$> arbitrary
