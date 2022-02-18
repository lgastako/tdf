{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

module CSVTest where

import           Data.Frame.Prelude

import           Test.Tasty.Hspec

import qualified Data.Vec.Lazy             as Vec
import qualified Data.Frame.Typed.CSV      as CSV
import           Data.Frame.Typed                 ( Frame )
import qualified Data.Frame.Typed          as DF
import           Data.Frame.Typed.Examples        ( PersonFields )

spec_CSV :: Spec
spec_CSV =
  context "with example.csv" $ do
    df <- fromMaybe (panic "spec_CSV.1")
          <$> runIO (fromRight boom <$> CSV.fromHeadedCSV "data/example.csv")
    let _ = df :: Frame Nat6 Int PersonFields

    it "should have the right number of rows" $
      DF.nrows df `shouldBe` 6

    it "should have the right toVector" $
      Just (DF.toVec df)
        `shouldBe`
          Vec.fromList
            [ #name .== "John"     .+ #age .== 46
            , #name .== "Kaialynn" .+ #age .== 21
            , #name .== "Zeke"     .+ #age .== 51
            , #name .== "Eric"     .+ #age .== 46
            , #name .== "Sean"     .+ #age .== 47
            , #name .== "Gandalf"  .+ #age .== 147
            ]

    it "should toTexts properly" $
      DF.toTexts df `shouldBe`
        [ ["", "age","name"]
        , ["0", "46","John"]
        , ["1", "21","Kaialynn"]
        , ["2", "51","Zeke"]
        , ["3", "46","Eric"]
        , ["4", "47","Sean"]
        , ["5", "147","Gandalf"]
        ]

boom :: a
boom = panic "spec_CSV explode"
