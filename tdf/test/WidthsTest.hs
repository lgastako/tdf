{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

module WidthsTest
  ( spec_Widths
  , example1
  , expected1
  ) where

import Data.Frame.Prelude

import Data.Frame.Typed.Types.Widths ( unWidths
                                     , widths
                                     )
import Test.Tasty.Hspec              ( Spec
                                     , context
                                     , it
                                     , shouldBe
                                     )

import qualified Data.Frame.Typed.Types.Widths as Widths

spec_Widths :: Spec
spec_Widths = do
  context "with empty list" $ do
    let ws = widths []

    it "should have no widths" $
      unWidths ws
        `shouldBe` []

    it "should return no columns for each row" $
      map (Widths.pad ws) example1
        `shouldBe` [ [], [], [] ]

    it "should fill nothing" $
      Widths.fill ws "-"
        `shouldBe` []

  context "from example 1" $ do
    let ws = widths example1
    it "should discover the correct widths" $
      unWidths ws
        `shouldBe` [8, 7]

    it "should pad properly" $
      map (Widths.pad ws) example1
        `shouldBe` expected1

    it "should fill properly" $
      Widths.fill ws "-"
        `shouldBe` [ "--------"
                   , "-------"
                   ]

example1 :: [[Text]]
example1 =
  [ ["name"    , "age val"]
  , ["Jonathan", "46"]
  , ["Dave"    , "52"]
  ]

expected1 :: [[Text]]
expected1 =
  [ ["    name", "age val"]
  , ["Jonathan", "     46"]
  , ["    Dave", "     52"]
  ]
