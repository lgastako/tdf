{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TableTest where

import           TDF.Prelude

import qualified Data.Text       as Text
import           TDF.Types.Table          ( Row( Row ) )
import qualified TDF.Types.Table as Table
import           Test.Tasty.Hspec
import           WidthsTest               ( example1 )

spec_Tables :: Spec
spec_Tables = do
  context "with example1" $ do
    let t = Table.fromTexts example1

    it "should render properly" $ do
      Table.render t
        `shouldBe` Text.unlines
          [ "name     | age val"
          , "Jonathan | 46     "
          , "Dave     | 52     "
          ]

  context "with example1 with headers" $ do
    let Just t = Table.fromHeadedRows (map Row example1)

    it "should render properly" $ do
      Table.render t
        `shouldBe` Text.unlines
          [ "name     | age val"
          , "---------+--------"
          , "Jonathan | 46     "
          , "Dave     | 52     "
          ]
