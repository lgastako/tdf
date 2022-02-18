{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module FrameTest where

import           Data.Frame.Prelude

import           Test.Tasty.Hspec

import qualified Data.Vec.Lazy              as Vec
import           Data.Frame.Typed                       ( Frame )
import qualified Data.Frame.Typed           as DF
import qualified Data.Frame.Typed.Series    as Series
import qualified Data.Frame.Typed.Examples  as Examples

type Animal = "animal" .== Text

spec_Frame :: Spec
spec_Frame = do
  context "with animals" $ do
    let df :: Frame Nat9 Int Animal
        df = Examples.animals

    it "should have the right shape" $ DF.shape df `shouldBe` (9, 1)

    it "at 1 #animal" $ DF.at 1 #animal df `shouldBe` Just "bee"
    it "at 5 #animal" $ DF.at 5 #animal df `shouldBe` Just "parrot"

    it "head 3" $ do
      let actual :: Frame Nat3 Int Animal
          actual = DF.head df
      (map (.! #animal) . DF.toList $ actual)
        `shouldBe` ["alligator", "bee", "falcon"]

    -- it "head -3" -- No longer possible - but should it be?

    it "tail 3" $ do
      let actual :: Frame Nat3 Int Animal
          actual = DF.tail df
      (map (.! #animal) . DF.toList $ actual)
        `shouldBe` ["shark", "whale", "zebra"]

    -- it "tail -3"   == No longer possible - but should it be?

    it "toList should produce proper results" $
      DF.toList df
        `shouldBe`
          [ #animal .== "alligator"
          , #animal .== "bee"
          , #animal .== "falcon"
          , #animal .== "lion"
          , #animal .== "monkey"
          , #animal .== "parrot"
          , #animal .== "shark"
          , #animal .== "whale"
          , #animal .== "zebra"
          ]

  context "with df1 (Person(name,age))" $ do
    let df = Examples.df1

    it "onColumn #age Vector.sum" $
      DF.onColumn #age Vec.sum df
        `shouldBe` 68

    context "at 0" $ do
      let idx = 0

      it "at 0 #name" $
        DF.at idx #name df
          `shouldBe` Just "Alex"

    context "at 1" $ do
      let idx = 1
      it "at 1 #name" $
        DF.at idx #name df
          `shouldBe` Just "Dave"

      it "at 1 #age" $
        DF.at idx #age df
          `shouldBe` Just 45

    it "toList should produce proper results" $
      DF.toList df
        `shouldBe`
          [ #age .== 23 .+ #name .== "Alex"
          , #age .== 45 .+ #name .== "Dave"
          ]

    it "toVector should produce proper results" $
      Just (DF.toVec df)
        `shouldBe`
          Vec.fromList
          [ #age .== 23 .+ #name .== "Alex"
          , #age .== 45 .+ #name .== "Dave"
          ]

    context "with a lens to the #age series" $ do

      it "should read" $
        Just (df ^. DF.series #age)
          `shouldBe` (Series.fromList [23, 45]) --  <&> #sName ?~ "age")

      it "should write" $
        (df & DF.series #age . each *~ 100)
          `shouldBe` Examples.df1Times100
