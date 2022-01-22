{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

module CSVTest where

import           TDF.Prelude

import           Test.Tasty.Hspec

import qualified Data.Vector      as Vector
import qualified TDF.CSV          as CSV
import           TDF.DataFrame              ( DataFrame )
import qualified TDF.DataFrame    as DF
import           TDF.Examples               ( PersonFields )

spec_CSV :: Spec
spec_CSV = do
  context "with example.csv" $ do
    df <- runIO $ (fromRight explode <$> CSV.fromHeadedCSV "example.csv")
    let _ = df :: DataFrame Int PersonFields

    it "should have the right number of rows" $
      DF.nrows df `shouldBe` 6

    it "should have the right toVector" $
      DF.toVector df
        `shouldBe`
          Vector.fromList
            [ #age .== 46 .+ #name .== "John"
            , #age .== 21 .+ #name .== "Kaialynn"
            , #age .== 51 .+ #name .== "Zeke"
            , #age .== 46 .+ #name .== "Eric"
            , #age .== 47 .+ #name .== "Sean"
            , #age .== 147 .+ #name .== "Gandalf"
            ]

    it "should toTexts prooperly" $
      DF.toTexts df `shouldBe`
        [ ["age","name"]
        , ["46","John"]
        , ["21","Kaialynn"]
        , ["51","Zeke"]
        , ["46","Eric"]
        , ["47","Sean"]
        , ["147","Gandalf"]
        ]

explode :: a
explode = panic "spec_CSV explode"
