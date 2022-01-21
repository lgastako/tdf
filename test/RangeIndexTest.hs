{-# LANGUAGE NoImplicitPrelude #-}

module RangeIndexTest where

import           TDF.Prelude

import           Test.Tasty.Hspec

import           TDF.Types.RangeIndex               ( stepping
                                                    , through
                                                    , upTo
                                                    )
import qualified TDF.Types.Index      as Index
import           TDF.Types.RangeIndex               ( RangeIndex )
import qualified TDF.Types.RangeIndex as RangeIndex

-- Examples from
-- https://cs.stanford.edu/people/nick/py/python-range.html
-- some of the examples seem to be wrong so I've checked them against python

spec_RangeIndex :: Spec
spec_RangeIndex = do
  context "upTo" $ do

    it "0" $ do
      RangeIndex.toList (upTo 0)
        `shouldBe` ([] :: [Int])

    it "1" $ do
      RangeIndex.toList (upTo 1)
        `shouldBe` [0 :: Int]

    it "5" $ do
      RangeIndex.toList (upTo (5 :: Int))
        `shouldBe` [0, 1, 2, 3, 4 :: Int]

    it "negative number" $ do
      RangeIndex.toList (upTo (-42))
        `shouldBe` ([] :: [Int])

  context "through" $ do

    it "3,5" $ do
      RangeIndex.toList (3 `through` 5)
        `shouldBe` [3, 4 :: Int]

  context "stepping" $ do

    it "0 3 1" $ do
      RangeIndex.toList (stepping 0 3 1)
        `shouldBe` [0, 1, 2 :: Int]

    context "with a negative step" $ do
      it "10 5 -1" $ do
        RangeIndex.toList (stepping 10 5 (-1))
          `shouldBe` [10, 9, 8, 7, 6 :: Int]

      it "6 5 -2" $ do
        RangeIndex.toList (stepping 6 5 (-2))
          `shouldBe` [6 :: Int]

      it "5 5 -2 (equal to stop is omitted)" $ do
        RangeIndex.toList (stepping 5 5 (-2))
          `shouldBe` ([] :: [Int])

      it "4 5 -2 (beyiond the stop is omitted)" $ do
        RangeIndex.toList (stepping 4 5 (-2))
          `shouldBe` ([] :: [Int])

  context "Index" $ do
    let ri = RangeIndex.upTo 10 :: RangeIndex Int

    it ".start" $
      Index.start ri
        `shouldBe` (0 :: Int)

    it ".stop" $
      Index.stop ri
        `shouldBe` (10 :: Int)

    it ".next" $
      fmap (\n -> Index.next (RangeIndex.upTo (10 :: Int)) (n :: Int)) [0..10]
        `shouldBe` [1..11]
