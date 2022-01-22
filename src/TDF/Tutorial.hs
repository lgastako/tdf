{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module TDF.Tutorial where

import TDF.Prelude

import           TDF.DataFrame        ( DataFrame )
import qualified TDF.CSV       as CSV

type PersonFields = NameFields .+ AgeFields

type VecPersonFields =
  (  "name" .== Vector Text
  .+ "age"  .== Vector Int
  )

type    Person = Rec    PersonFields
type VecPerson = Rec VecPersonFields

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

readExamplesIO :: IO (DataFrame Int PersonFields)
readExamplesIO = either explode identity <$> CSV.fromHeadedCSV "example.csv"
  where
    explode error = panic . show $ error
