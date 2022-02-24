{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Grid.Index.RangeTest
  ( spec_Range
  ) where

import Data.Grid.Prelude

import Test.Tasty.Hspec    ( Spec
                           , context
                           , hspec
                           , it
                           , shouldBe
                           )
import Data.Vector.Sized.X ( rangeParts )

import qualified Data.Vector.Sized.X as Sized

_ = hspec

spec_Range :: Spec
spec_Range = do
  context "given a vector of [0, 1, 2]" $ do
    let v :: Sized.Vector 3 Int
        Just v = Sized.fromList [0, 1, 2]

    it "should view the right rangeParts" $
      v ^? rangeParts `shouldBe` Just (1, (0, 2))

  context "given a vector of [1, 2, 3]" $ do
    let v :: Sized.Vector 3 Int
        Just v = Sized.fromList [1, 2, 3]

    it "should view the right rangeParts" $
      v ^? rangeParts `shouldBe` Just (1, (1, 3))

  context "given a vector of [1, 5, 3, 12]" $ do
    let v :: Sized.Vector 4 Int
        Just v = Sized.fromList [1, 5, 2, 12]

    it "should find no rangeParts" $
      v ^? rangeParts `shouldBe` Nothing
