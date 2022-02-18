{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Orphans () where

import Data.Frame.Prelude

import Data.Text.Arbitrary ()
import Test.QuickCheck

import           Data.Frame.Typed.Index            ( Index )
import qualified Data.Frame.Typed.Index  as Index
import           Data.Frame.Typed.Series           ( Series )
import qualified Data.Frame.Typed.Series as Series

instance ( Arbitrary a
         , Arbitrary (Series.Options n idx a)
         ) => Arbitrary (Series n idx a) where
  arbitrary = Series.construct <$> arbitrary

instance ( Arbitrary a
         , Arbitrary idx
         , SNatI n
         ) => Arbitrary (Series.Options n idx a) where
  arbitrary = Series.Options
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance ( Arbitrary idx
         , SNatI n
         ) => Arbitrary (Index n idx) where
  arbitrary = Index.fromVec <$> arbitrary
