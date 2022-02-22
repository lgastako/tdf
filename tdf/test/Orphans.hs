{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Orphans () where

import Data.Frame.Prelude

import Data.Frame.Typed.Index  ( Index )
import Data.Frame.Typed.Series ( Series )
import Data.Frame.Typed.Name   ( Name )
import Data.Text.Arbitrary     ()
import Test.QuickCheck         ( Arbitrary(..) )

import qualified Data.Frame.Typed.Index  as Index
import qualified Data.Frame.Typed.Series as Series
import qualified Data.Frame.Typed.Name   as Name

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

instance Arbitrary Name where
  arbitrary = maybe arbitrary pure . Name.fromText =<< arbitrary
