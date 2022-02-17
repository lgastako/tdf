{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module TDF.Tutorial where

import TDF.Prelude

import qualified TDF.CSV       as CSV
import           TDF.DataFrame           ( DataFrame )
import qualified TDF.DataFrame as DF
import qualified TDF.Series    as Series

type PersonFields = NameField .+ AgeField .+ LocationField

type VecPersonFields (n :: Nat) =
  (  "name"     .== Vec n Text
  .+ "age"      .== Vec n Int
  .+ "location" .== Vec n Text
  )

type    Person   = Rec     PersonFields
type VecPerson n = Rec (VecPersonFields n)

type PlayerFields =
  (  "name"     .== Text
  .+ "team"     .== Text
  )

type Player = Rec PlayerFields

type AgeField = "age" .== Int

type AgeRec = Rec AgeField

type NameField = "name" .== Text

type LocationField = "location" .== Text

type FullNameField = "fullName" .== Text

type FullPersonFields = FullNameField .+ AgeField .+ LocationField

type FullPerson = Rec FullPersonFields

type NameRec = Rec NameField

seriesFromDF :: IO ()
seriesFromDF = withExamples $ \df -> do
  nl
  DF.display df
  let series = df ^. DF.series #age
  nl
  Series.display series
  nl

headDemo :: IO ()
headDemo = withExamples $ \df -> do
  nl
  DF.display df
  let top3 :: DataFrame Nat3 Int PersonFields
      top3 = DF.head df
  nl
  DF.display top3
  nl

popDemo :: IO ()
popDemo = withExamples $ \df -> do
  nl
  DF.display df
  let (df', s) = DF.pop #age df
  nl
  DF.display df'
  nl
  Series.display s

-- ================================================================ --
--  Helpers
-- ================================================================ --
readExamplesIO :: SNatI n
               => IO (Maybe (DataFrame n Int PersonFields))
readExamplesIO = either boom identity <$> CSV.fromHeadedCSV "data/example.csv"
  where
    boom = explode . show

withExamples :: (DataFrame Nat6 Int PersonFields -> IO ())
             -> IO ()
withExamples f = readExamplesIO >>= \case
  Nothing -> panic "invalid csv - maybe not 7 rows (6 + header)?"
  Just examples -> f examples
