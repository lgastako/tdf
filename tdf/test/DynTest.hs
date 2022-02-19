{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DynTest
  ( spec_Dyn
  ) where

import Data.Frame.Prelude

import Data.Dynamic       ( fromDynamic
                          , toDyn
                          )
import Test.Tasty.Hspec   ( Spec
                          , context
                          , it
                          , shouldBe
                          )

import qualified Data.Frame.Typed.Utils.Dyn as Dyn
import qualified Data.HashMap.Strict        as HashMap

spec_Dyn :: Spec
spec_Dyn = do
  context "fromAnyDyn" $ do

    context "given a dyn with a Text in it" $ do
      let d = toDyn ("foo" :: Text)

      it "should be able to read text" $
        Dyn.fromAnyDyn d
          `shouldBe` Just "foo"

    context "given a dyn with an Int in it" $ do
      let d = toDyn (5 :: Int)

      it "should be able to read text" $
        Dyn.fromAnyDyn d
          `shouldBe` Just "5"

    context "given a dyn with a Double in it" $ do
      let d = toDyn (5 :: Double)

      it "should be able to read text" $
        Dyn.fromAnyDyn d
          `shouldBe` Just "5.0"

  context "getValue" $

    context "given a map with a Text in it" $ do
      let k = "someKey"
          v = "foo" :: Text
          d = toDyn v
          m = HashMap.singleton k d

      it "should be able to get Text right back" $
        fromDynamic d
          `shouldBe` Just v

      it "should have a dyn at the key" $
        (() <$ HashMap.lookup k m)
          `shouldBe` Just ()

      it "should be able to get Text manually" $ do
        let Just dyn = HashMap.lookup k m
        fromDynamic dyn
          `shouldBe` Just v

      it "should be able to read Text" $
        Dyn.getValue k m
          `shouldBe` v
