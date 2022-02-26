{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module RectTest
  ( spec_Rect
  ) where

import Data.Grid.Prelude

import Orphans           ()
import Test.Tasty.Hspec  ( Spec
                         , context
                         , hspec
                         , it
                         , shouldBe
                         )
import Data.Rect       ( (<+>)
                         , (<//>)
                         , Rect
                         )

import qualified Data.Char as C
import qualified Data.Rect as R

_ = hspec

spec_Rect :: Spec
spec_Rect =  do
  context "given a (Rect 0 0)" $ do
    let sq = R.empty :: Rect 0 0 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    it "should have toTexts = []" $
      R.toTexts sq `shouldBe` []

    it "should have toList = []" $
      R.toList sq `shouldBe` []

    it "should remain same Rect under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should be it's own transpose" $
      R.transpose sq `shouldBe` sq

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` sq

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` sq

    context "transpose" $
      it "should tranpose to itself " $
        R.transpose sq `shouldBe` sq

  context "given a (Rect 0 N)" $ do
    let sq = truth :: R.Rect 0 3 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    it "should have toTexts = [ [], [], [] ]" $
      R.toTexts sq `shouldBe` [ [], [], [] ]

    it "should have toList []" $
      toList sq `shouldBe` []

    it "should remain same Rect under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` (truth :: Rect 0 6 Bool)

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` sq

    context "transpose" $
      it "should transpose dimensions " $
        R.transpose sq `shouldBe` (truth :: R.Rect 3 0 Bool)

  context "given a (Rect N 0)" $ do
    let sq = truth :: R.Rect 3 0 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    it "should toTexts []" $
      R.toTexts sq `shouldBe` []

    it "should toList []" $
      R.toList sq `shouldBe` []

    it "should remain same Rect under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` (truth :: R.Rect 6 0 Bool)

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` sq

    context "transpose" $
      it "should transpose dimensions " $
        R.transpose sq `shouldBe` (truth :: R.Rect 0 3 Bool)

  context "given a (Rect N M) of homogenous values" $ do
    let sq = truth :: R.Rect 2 2 Bool

    it "should have length 0" $
      length sq `shouldBe` 4

    it "should toTexts []" $
      R.toTexts sq `shouldBe` show <<$>> [[True, True], [True, True]]

    it "should toList []" $
      R.toList sq `shouldBe` [True, True, True, True]

    it "should remain the same Rect under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should be it's own transpose" $
      R.transpose sq `shouldBe` sq

    it "should index values properly" $
      sq ^. R.at (0, 1) `shouldBe` True

  context "given a (Rect N M) of heterogenous values" $ do
    let xs = [[True, False], [False, True]]
        sq = (R.fromList xs
              & fromMaybe (panic "RectTest:boom")) :: R.Rect 2 2 Bool

    it "should have length 0" $
      length sq `shouldBe` 4

    it "should toTexts []" $
      R.toTexts sq `shouldBe` show <<$>> xs

    it "should toList []" $
      R.toList sq `shouldBe` concat xs

    it "should remain the same Rect under Applicative" $
      ((&&) <$> sq <*> R.transpose sq) `shouldBe` sq

    it "should index values properly" $
      ( sq ^. R.at (0, 1)
      , sq ^. R.at (1, 0)
      , sq ^. R.at (0, 0)
      , sq ^. R.at (1, 1)
      ) `shouldBe` (False, False, True, True)

  context "given a (Rect 4 4) of heterogenous values" $ do
    let xs = [ "ABCD"
             , "EFGH"
             , "IJKL"
             , "MNOP"
             ]
        ys = [ "CD"
             , "GH"
             , "KL"
             , "OP"
             ]
        zs = [ [ 'E', 'F', 'G', 'H' ]
             , [ 'I', 'J', 'K', 'L' ]
             ]
        qs = [ [ 'F', 'G' ]
             , [ 'J', 'K' ]
             ]
        sqx = R.unsafeFromList xs :: R.Rect 4 4 Char
        sqy = R.unsafeFromList ys :: R.Rect 4 2 Char
        sqz = R.unsafeFromList zs :: R.Rect 2 4 Char
        sqq = R.unsafeFromList qs :: R.Rect 2 2 Char

    context "sliceC" $ do
      it "should something or other..." $ do
        let sub :: Rect 4 2 Char
            sub = R.sliceC (Proxy @2) sqx
        sub `shouldBe` sqy

    context "sliceR" $ do
      it "should something or other..." $ do
        let sub :: Rect 2 4 Char
            sub = R.sliceR (Proxy @1) sqx
        sub `shouldBe` sqz

    context "slice" $ do

      context "viewing" $ do
        it "should work as expected" $ do
          let sub :: Rect 2 2 Char
              sub = sqx ^. R.slice (Proxy @1, Proxy @1)
          sub `shouldBe` sqq

      context "setting" $ do
        it "should work as expected"$ do
          let result :: Rect 4 4 Char
              result = R.unsafeFromList
                [ "ABCD"
                , "EfgH"
                , "IjkL"
                , "MNOP"
                ]
          (sqx & R.slice @_ @_ @2 @2 (Proxy @1, Proxy @1) %~ fmap C.toLower)
            `shouldBe` result

truth :: Applicative f => f Bool
truth = pure True
