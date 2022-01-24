{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module TDF.Examples where

import           TDF.Prelude                hiding ( drop
                                                   , take
                                                   )

import qualified Data.Row.Records as Rec
import qualified Data.Text        as Text
import qualified Data.Vec.Lazy    as Vec
import           TDF.DataFrame                     ( Axes
                                                   , DataFrame
                                                   )
import qualified TDF.CSV          as CSV
import qualified TDF.DataFrame    as DF
import qualified TDF.Options      as Options
import           System.IO.Unsafe                  ( unsafePerformIO )

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

data Person' = Person'
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

personName1 :: NameRec
personName1 = #name .== "John"

justName :: Person -> NameRec
justName p = #name .== (p .! #name)

person :: Person
person = #name .== "Alex"
      .+ #age .== 23

person2 :: Person
person2 = #age  .== 45
       .+ #name .== "Dave"

greet :: (r ≈ "name" .== Text)
      => Rec r
      -> Text
greet = ("Hello " <>) . (.! #name)

df1 :: DataFrame Nat2 Int PersonFields
df1 = DF.fromList
  [ person
  , person2
  ]
  & fromMaybe (panic "Examples.df1")

df1Renamed :: DataFrame Nat2 Int FullPersonFields
df1Renamed = DF.rename #name #fullName df1

-- TODO can we make it easy to do the same thing with just "age" (or #age) to
--      avoid having to know the type... where the return type is whatever the
--      type of "age" is in in df1?  maybe we don't want to lose the type info?
--      so perhaps DF.restrict replaces both select_types and whatever other
--      subsetting facilities? let's assume so for now.
ages :: DataFrame Nat2 Int ("age" .== Int)
ages = DF.restrict df1

df1Axes :: Axes Int
df1Axes = DF.axes df1

-- Neat.
df1Restricted :: DataFrame Nat2 Int NameFields
df1Restricted = DF.restrict df1

df1' :: DataFrame Nat2 Int PersonFields
df1' = DF.reindex [0, 1] df1

-- λ> DF.memSize df1'
-- 1096
-- Yeesh that's a lot of overhead for [{name:Alex,age:23},{name:Dave,age:45}]

df1'' :: DataFrame Nat2 Int (PersonFields .+ "foo" .== Text)
df1'' = DF.map (\x -> x .+ #foo .== ("bar" ::Text)) df1'

df2 :: DataFrame Nat2 Int NameFields
df2 = DF.map justName df1

df3' :: DataFrame Nat2 Int (PersonFields .+ "fullName" .== Text)
df3' = DF.map plusFull df1

df3 :: DataFrame Nat2 Int NameFields
df3 = DF.column #name df1

-- df6 :: DataFrame Int PersonFields
-- df6 = DF.fromNativeVector nativeVector

nativeVector :: Vec Nat3 Person'
nativeVector = Vec.fromList
  [ Person' "bob"  86
  , Person' "dave" 55
  , Person' "john" 46
  ]
  & fromMaybe (panic "nativeVector")

-- something :: VecPerson
-- something = Rec.distribute . DF.toVec $ df6

-- person's :: [Person']
-- person's = Vec.toList . DF.toNativeVec $ df6

-- Example from
--   https://pandas.pydata.org/pandas-docs/version/0.23.0/generated/pandas.Series.tail.html
animals :: DataFrame Nat9 Int ("animal" .== Text)
animals = DF.construct $ Options.fromVec v
  where
    v :: Vec Nat9 (Rec ("animal" .== Text))
    v = (Vec.fromList . map mkRec $ animalNames)
          & fromMaybe (panic "Examples.animals.1")

    mkRec a = #animal .== a

    animalNames =
      [ "alligator"
      , "bee"
      , "falcon"
      , "lion"
      , "monkey"
      , "parrot"
      , "shark"
      , "whale"
      , "zebra"
      ]

rendered :: Text
rendered = Text.unlines
  [ DF.render df1
  , DF.render df2
  ]

ageToText :: Rec AgeFields -> [Text]
ageToText = pure . show . (.! #age)

toTexts :: Rec PersonFields -> [Text]
toTexts rp =
  [ rp .! #name
  , show (rp .! #age)
  ]

toTexts' :: Rec NameFields -> [Text]
toTexts' rp = [ rp .! #name ]

indexTest :: Bool
indexTest = Just (DF.index df1) == Vec.fromList [0, 1]

rd :: IO ()
rd = putStr rendered

displayDf1 :: IO ()
displayDf1 = DF.display df1

flagged :: DataFrame Nat2 Int (PersonFields .+ "flagged" .== Bool)
flagged = DF.extend #flagged False df1

plusFull :: Rec PersonFields
         -> Rec (PersonFields .+ "fullName" .== Text)
plusFull r = Rec.extend #fullName fname r
  where
    fname = r .! #name

capitalize :: SNatI n
           => DataFrame n Int PersonFields
           -> DataFrame n Int (PersonFields .+ "capsName" .== Text)
capitalize = DF.extendWith #capsName (\r -> Text.toUpper $ r .! #name)

{-# NOINLINE examples #-}
examples :: DataFrame Nat6 Int PersonFields
examples = unsafePerformIO $ CSV.fromHeadedCSV "example.csv" >>= \case
  Left error      -> panic $ "Examples.examples.1: " <> show error
  Right Nothing   -> panic $ "Examples.examples.2: Nothing"
  Right (Just df) -> pure df
