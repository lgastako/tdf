{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module TDF.Tutorial where

import TDF.Prelude

import qualified TDF.CSV       as CSV
import           TDF.DataFrame           ( DataFrame )
import qualified TDF.DataFrame as DF
import qualified TDF.Series    as Series

type PersonFields = NameFields .+ AgeFields

type VecPersonFields (n :: Nat) =
  (  "name" .== Vec n Text
  .+ "age"  .== Vec n Int
  )

type    Person   = Rec     PersonFields
type VecPerson n = Rec (VecPersonFields n)

type PlayerFields =
  (  "name" .== Text
  .+ "team" .== Text
  )

type Player = Rec PlayerFields

type AgeFields = "age" .== Int

type AgeRec = Rec AgeFields

type NameFields = "name" .== Text

type FullNameFields = "fullName" .== Text

type FullPersonFields = FullNameFields .+ AgeFields

type FullPerson = Rec FullPersonFields

type NameRec = Rec NameFields

readExamplesIO :: SNatI n
               => IO (Maybe (DataFrame n Int PersonFields))
readExamplesIO = either explode identity <$> CSV.fromHeadedCSV "example.csv"
  where
    explode error = panic . show $ error

seriesFromDF :: IO ()
seriesFromDF = readExamplesIO >>= \case
  Nothing -> panic "invalid csv - maybe not 7 rows (6 + header)?"
  Just (examples :: DataFrame Nat6 Int PersonFields) -> do
    nl
    DF.display examples
    let series = DF.series #age examples
    nl
    Series.display series

nl :: IO ()
nl = putText ""
