{-# LANGUAGE OverloadedLabels #-}

module RT.DataFrameTest where

import           Test.Tasty.Hspec

import           Control.Lens               ( view )
import           Data.Generics.Labels       ()
import qualified RT.DataFrame         as DF
import           RT.Examples                ( animals )

spec_DataFrame :: Spec
spec_DataFrame = do
  context "with animals" $ do
    let df = animals

    it "should have the right shape" $ do
      DF.shape df
        `shouldBe` (9, 1)

    it "head_" $ do
      (map (view #animal) . DF.toList . DF.head_ $ df)
        `shouldBe` ["alligator", "bee", "falcon", "lion", "monkey"]

    it "head 3" $ do
      (map (view #animal) . DF.toList . DF.head 3 $ df)
        `shouldBe` ["alligator", "bee", "falcon"]

    it "head -3" $ do
      (map (view #animal) . DF.toList . DF.head (-3) $ df)
        `shouldBe` ["alligator", "bee", "falcon", "lion", "monkey", "parrot"]

    it "tail_" $ do
      (map (view #animal) . DF.toList . DF.tail_ $ df)
        `shouldBe` ["monkey", "parrot", "shark", "whale", "zebra"]

    it "tail 3" $ do
      (map (view #animal) . DF.toList . DF.tail 3 $ df)
        `shouldBe` ["shark", "whale", "zebra"]

    it "tail -3" $ do
      (map (view #animal) . DF.toList . DF.tail (-3) $ df)
        `shouldBe` ["lion", "monkey", "parrot", "shark", "whale", "zebra"]

    it "at 1 #animal" $ do
      DF.at 1 #animal df
        `shouldBe` Just "bee"

    it "at 5 #animal" $ do
      DF.at 5 #animal df
        `shouldBe` Just "parrot"
