{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Index.RangeTest
  ( spec_Range
  ) where

import Data.Frame.Prelude

import Data.Frame.Typed.Index.Range ( RangeIndex )
import Orphans                      ()
import Test.Tasty.Hspec

import qualified Data.Frame.Typed.Index.Range as RangeIndex
import qualified Data.Frame.Typed.SubIndex    as SubIndex
import qualified Data.Vec.Lazy.X              as Vec

spec_Range :: Spec
spec_Range =
  context "defaultIntsFor 'bar'" $ do

    it "should have 0, 1, 2" $
      SubIndex.toLst mockIdx
        `shouldBe` [0, 1, 2]

    it "should append to itself correctly" $ do
      let Just ri = RangeIndex.append mockIdx mockIdx
      SubIndex.toLst ri
        `shouldBe` [0, 1, 2, 3, 4, 5]

mockData :: Vec Nat3 Bool
mockData = Vec.repeat True

mockIdx :: RangeIndex Nat3 Int
mockIdx = RangeIndex.defaultIntsFor mockData
