{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Index.RangeTest where

import           Data.Frame.Prelude

import qualified Data.Frame.Typed.Index.Range as RangeIndex
import           Data.Frame.Typed.Index.Range               ( RangeIndex )
import qualified Data.Frame.Typed.SubIndex    as SubIndex
import qualified Data.Vec.Lazy.X              as Vec
import           Test.Tasty.Hspec
import           Orphans                                    ()

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
