{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Orphans () where

import Data.Text.Arbitrary()
import TDF.Prelude
import Test.QuickCheck

import           TDF.Index            ( Index )
import qualified TDF.Index  as Index
import           TDF.Series           ( Series )
import qualified TDF.Series as Series

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
