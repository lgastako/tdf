{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Grid.SeriesTest
  ( spec_Series
  ) where

import Data.Grid.Prelude

import Data.Grid.Series     ( Series )
import Test.Tasty.Hspec     ( Spec
                            , context
                            , hspec
                            , it
                            , shouldBe
                            )

import qualified Data.Grid.Index  as I
import qualified Data.Grid.Series as S

_ = hspec

spec_Series :: Spec
spec_Series = do
  context "with an empty unit series" $ do
    let series :: Series 0 Int ()
        series = S.empty

    it "should have length of 0" $
      length series `shouldBe` 0

    it "should have an empty index" $
      series ^. S.index `shouldBe` I.empty

    it "should still be empty after operations" $
      S.op const () series `shouldBe` series

    it "(the last thing) should be tested better than 'const ()'" $ do
      S.op (+) (5 :: Int) S.empty `shouldBe` (S.empty :: Series 0 Int Int)

    it "should render properly" $
      S.toTexts series `shouldBe` ("series", [])

    context "appended to itself" $ do
      let series2 :: Series 0 Int ()
          series2 = series S.++ series

      it "should have length of 0" $
        length series2 `shouldBe` 0

      it "should have an empty index" $
        series2 ^. S.index `shouldBe` I.empty

  context "with a 1-unit series" $ do
    let series :: Series 1 Int ()
        series = S.single ()

    it "should have a length of 1" $
      length series `shouldBe` 1

    it "should have a default index" $
      series ^. S.index `shouldBe` I.fill

  context "with a 3-unit series" $ do
    let series :: Series 3 Int ()
        Just series = S.fromList $ replicate 3 ()

    it "should have a length of 3" $
      length series `shouldBe` 3

    it "should have a default index" $
      series ^. S.index `shouldBe` I.fill
