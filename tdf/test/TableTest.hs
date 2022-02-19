{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TableTest
  ( spec_Table
  ) where

import Data.Frame.Prelude

import Data.Frame.Typed.Types.Table ( Row( Row ) )
import Test.Tasty.Hspec             ( Spec
                                    , context
                                    , it
                                    , shouldBe
                                    )
import WidthsTest                   ( example1 )

import qualified Data.Text                    as Text
import qualified Data.Frame.Typed.Types.Table as Table

spec_Table :: Spec
spec_Table = do
  context "with example1" $ do
    let t = Table.fromTexts example1

    it "should render properly" $
      Table.render t
        `shouldBe` Text.unlines
          [ "    name | age val"
          , "Jonathan |      46"
          , "    Dave |      52"
          ]

    it "should promote properly" $
      (Table.render . fromMaybe (panic "ahh!") . Table.promoteHeader $ t)
        `shouldBe` Text.unlines
          [ "    name | age val"
          , "---------+--------"
          , "Jonathan |      46"
          , "    Dave |      52"
          ]

  context "with example1 with headers" $ do
    let Just t = Table.fromHeadedRows (map Row example1)

    it "should render properly" $
      Table.render t
        `shouldBe` Text.unlines
          [ "    name | age val"
          , "---------+--------"
          , "Jonathan |      46"
          , "    Dave |      52"
          ]
