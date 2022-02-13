{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DynTest where

import           TDF.Prelude

import           Data.Dynamic                   ( fromDynamic
                                                , toDyn
                                                )
import           Test.Tasty.Hspec
import qualified Data.HashMap.Strict as HashMap

import qualified TDF.Utils.Dyn       as Dyn

spec_Dyn :: Spec
spec_Dyn = do
  context "fromAnyDyn" $ do

    context "given a dyn with a Text in it" $ do
      let d = toDyn ("foo" :: Text)

      it "should be able to read text" $ do
        Dyn.fromAnyDyn d
          `shouldBe` Just "foo"

    context "given a dyn with an Intt in it" $ do
      let d = toDyn (5 :: Int)

      it "should be able to read text" $ do
        Dyn.fromAnyDyn d
          `shouldBe` Just "5"

  context "getValue" $ do

    context "given a map with a Text in it" $ do
      let k = "someKey"
          v = "foo" :: Text
          d = toDyn v
          m = HashMap.singleton k d

      runIO $ print ("m" :: Text, m)

      it "should be able to get Text right back" $
        fromDynamic (toDyn v)
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
