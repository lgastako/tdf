{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SquareTest
  ( spec_Square
  ) where

import Data.Grid.Prelude

import Orphans           ()
import Test.Tasty.Hspec  ( Spec
                         , context
                         , hspec
                         , it
                         , shouldBe
                         )
import Data.Square       ( (<+>)
                         , (<//>)
                         , Square
                         )

import qualified Data.Square as F

_ = hspec

spec_Square :: Spec
spec_Square =  do
  context "given a (Square 0 0)" $ do
    let sq = F.empty :: Square 0 0 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    it "should have toTexts = []" $
      F.toTexts sq `shouldBe` []

    it "should have toList = []" $
      F.toList sq `shouldBe` []

    it "should remain same Square under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should be it's own transpose" $
      F.transpose sq `shouldBe` sq

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` sq

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` sq

    context "transpose" $
      it "should tranpose to itself " $
        F.transpose sq `shouldBe` sq

  context "given a (Square 0 N)" $ do
    let sq = truth :: F.Square 0 3 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    it "should have toTexts = [ [], [], [] ]" $
      F.toTexts sq `shouldBe` [ [], [], [] ]

    it "should have toList []" $
      toList sq `shouldBe` []

    it "should remain same Square under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` (truth :: Square 0 6 Bool)

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` sq

    context "transpose" $
      it "should transpose dimensions " $
        F.transpose sq `shouldBe` (truth :: F.Square 3 0 Bool)

  context "given a (Square N 0)" $ do
    let sq = truth :: F.Square 3 0 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    it "should toTexts []" $
      F.toTexts sq `shouldBe` []

    it "should toList []" $
      F.toList sq `shouldBe` []

    it "should remain same Square under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` (truth :: F.Square 6 0 Bool)

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` sq

    context "transpose" $
      it "should transpose dimensions " $
        F.transpose sq `shouldBe` (truth :: F.Square 0 3 Bool)

  context "given a (Square N M) of homogenous values" $ do
    let sq = truth :: F.Square 2 2 Bool

    it "should have length 0" $
      length sq `shouldBe` 4

    it "should toTexts []" $
      F.toTexts sq `shouldBe` show <<$>> [[True, True], [True, True]]

    it "should toList []" $
      F.toList sq `shouldBe` [True, True, True, True]

    it "should remain the same Square under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should be it's own transpose" $
      F.transpose sq `shouldBe` sq

    it "should index values properly" $
      sq ^. F.at (0, 1) `shouldBe` True

  context "given a (Square N M) of heterogenous values" $ do
    let xs = [[True, False], [False, True]]
        sq = (F.fromList xs
              & fromMaybe (panic "SquareTest:boom")) :: F.Square 2 2 Bool

    it "should have length 0" $
      length sq `shouldBe` 4

    it "should toTexts []" $
      F.toTexts sq `shouldBe` show <<$>> xs

    it "should toList []" $
      F.toList sq `shouldBe` concat xs

    it "should remain the same Square under Applicative" $
      ((&&) <$> sq <*> F.transpose sq) `shouldBe` sq

    it "should index values properly" $
      ( sq ^. F.at (0, 1)
      , sq ^. F.at (1, 0)
      , sq ^. F.at (0, 0)
      , sq ^. F.at (1, 1)
      ) `shouldBe` (False, False, True, True)

truth :: Applicative f => f Bool
truth = pure True
