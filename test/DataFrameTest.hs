{-# LANGUAGE OverloadedLabels #-}

module DataFrameTest where

import           Test.Tasty.Hspec

import           Data.Row                       ( (.!) )
import           Data.Generics.Labels           ()
import qualified Data.Vector          as Vector
import qualified TDF.DataFrame        as DF
import qualified TDF.Examples         as Examples

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

  context "with df1 (Person(name,age))" $ do
    let df = Examples.df1

    it "onColumn #age Vector.sum" $
      DF.onColumn #age Vector.sum df
        `shouldBe` 68
