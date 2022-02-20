{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module NameTest
  ( spec_Name
  ) where

import Data.Frame.Prelude

import Test.Tasty.Hspec   ( Spec
                          , it
                          , shouldBe
                          )

import           Data.Frame.Typed.Name         ( Name )
import qualified Data.Frame.Typed.Name as Name

spec_Name :: Spec
spec_Name = do
  it "should combine two Nothings properly" $
    Name.combine Nothing Nothing
      `shouldBe` (Just $ un "[ >> ]")

  it "should combine Something/Nothing properly" $
    Name.combine (Just $ un "a") Nothing
      `shouldBe` Just (un "[a >> ]")

  it "should combine Nothing/Something properly" $
    Name.combine Nothing (Just $ un "b")
      `shouldBe` Just (un "[ >> b]")

  it "should combine two Somethings properly" $
    Name.combine (Just (un "a")) (Just $ un "b")
      `shouldBe` Just (un "[a >> b]")

un :: Text -> Name
un = Name.unsafeFromText
