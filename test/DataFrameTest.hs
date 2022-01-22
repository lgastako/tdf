{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

module DataFrameTest where

import           TDF.Prelude

import           Test.Tasty.Hspec

import qualified Data.Vector       as Vector
import qualified TDF.DataFrame     as DF
import qualified TDF.Examples      as Examples

spec_DataFrame :: Spec
spec_DataFrame = do
  context "with animals" $ do
    let df = Examples.animals

    it "should have the right shape" $ DF.shape df `shouldBe` (9, 1)

    it "at 1 #animal" $ DF.at 1 #animal df `shouldBe` Just "bee"
    it "at 5 #animal" $ DF.at 5 #animal df `shouldBe` Just "parrot"

    it "head_" $
      (map (.! #animal) . DF.toList . DF.head_ $ df)
        `shouldBe` ["alligator", "bee", "falcon", "lion", "monkey"]

    it "head 3" $
      (map (.! #animal) . DF.toList . DF.head 3 $ df)
        `shouldBe` ["alligator", "bee", "falcon"]

    it "head -3" $
      (map (.! #animal) . DF.toList . DF.head (-3) $ df)
        `shouldBe` ["alligator", "bee", "falcon", "lion", "monkey", "parrot"]

    it "tail_" $
      (map (.! #animal) . DF.toList . DF.tail_ $ df)
        `shouldBe` ["monkey", "parrot", "shark", "whale", "zebra"]

    it "tail 3" $
      (map (.! #animal) . DF.toList . DF.tail 3 $ df)
        `shouldBe` ["shark", "whale", "zebra"]

    it "tail -3" $
      (map (.! #animal) . DF.toList . DF.tail (-3) $ df)
        `shouldBe` ["lion", "monkey", "parrot", "shark", "whale", "zebra"]

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
      DF.onColumn #age Vector.sum df
        `shouldBe` 68

    it "at 0 #name" $
      DF.at 0 #name df
        `shouldBe` Just "Alex"

    it "at 1 #name" $
      DF.at 1 #name df
        `shouldBe` Just "Dave"

    it "at 1 #age" $
      DF.at 1 #age df
        `shouldBe` Just 23

    it "toList should produce proper results" $
      DF.toList df
        `shouldBe`
          [ #age .== 23 .+ #name .== "Alex"
          , #age .== 45 .+ #name .== "Alex"
          ]

    it "toVector should produce proper results" $
      DF.toVector df
        `shouldBe`
          Vector.fromList
            [ #age .== 23 .+ #name .== "Alex"
            , #age .== 45 .+ #name .== "Alex"
            ]
