{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

module CSVTest where

import           TDF.Prelude

import           Test.Tasty.Hspec

import qualified Data.Vec.Lazy    as Vec
import qualified TDF.CSV          as CSV
import           TDF.DataFrame              ( DataFrame )
import qualified TDF.DataFrame    as DF
import           TDF.Examples               ( PersonFields )

spec_CSV :: Spec
spec_CSV = do
  context "with example.csv" $ do
    df <- fromMaybe (panic "spec_CSV.1")
          <$> runIO (fromRight boom <$> CSV.fromHeadedCSV "data/example.csv")
    let _ = df :: DataFrame Nat6 Int PersonFields

    it "should have the right number of rows" $
      DF.nrows df `shouldBe` 6

    it "should have the right toVector" $
      Just (DF.toVec df)
        `shouldBe`
          Vec.fromList
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

boom :: a
boom = panic "spec_CSV explode"
