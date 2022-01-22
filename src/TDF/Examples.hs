{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module TDF.Examples where

import           TDF.Prelude                hiding ( drop
                                                   , take
                                                   )

import qualified Data.Row.Records as Rec
import qualified Data.Text        as Text
import qualified Data.Vector      as Vector
import           TDF.DataFrame                     ( Axes
                                                   , DataFrame
                                                   )
import qualified TDF.DataFrame    as DF

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

df1 :: DataFrame Int PersonFields
df1 = DF.fromList
  [ person
  , person2
  ]

df1Renamed :: DataFrame Int FullPersonFields
df1Renamed = DF.rename #name #fullName df1

-- TODO can we make it easy to do the same thing with just "age" (or #age) to
--      avoid having to know the type... where the return type is whatever the
--      type of "age" is in in df1?  maybe we don't want to lose the type info?
--      so perhaps DF.restrict replaces both select_types and whatever other
--      subsetting facilities? let's assume so for now.
ages :: DataFrame Int ("age" .== Int)
ages = DF.restrict df1

df1Axes :: Axes Int
df1Axes = DF.axes df1

-- Neat.
df1Restricted :: DataFrame Int NameFields
df1Restricted = DF.restrict df1

df1' :: DataFrame Int PersonFields
df1' = DF.reindex [0, 1] df1

-- λ> DF.memSize df1'
-- 1096
-- Yeesh that's a lot of overhead for [{name:Alex,age:23},{name:Dave,age:45}]

df1'' :: DataFrame Int (PersonFields .+ "foo" .== Text)
df1'' = DF.map (\x -> x .+ #foo .== ("bar" ::Text)) df1'

df2 :: DataFrame Int NameFields
df2 = DF.map justName df1

df3' :: DataFrame Int (PersonFields .+ "fullName" .== Text)
df3' = DF.map plusFull df1

df3 :: DataFrame Int NameFields
df3 = DF.column #name df1

df6 :: DataFrame Int PersonFields
df6 = DF.fromNativeVector nativeVector

nativeVector :: Vector Person'
nativeVector = Vector.fromList
  [ Person' "bob"  86
  , Person' "dave" 55
  , Person' "john" 46
  ]

something :: VecPerson
something = Rec.distribute . DF.toVector $ df6

person's :: [Person']
person's = Vector.toList . DF.toNativeVector $ df6

animals :: DataFrame Int ("animal" .== Text)
animals = DF.construct o
  where
    o = DF.opts
      { DF.optData    = Vector.fromList . map mkRec $ animalNames
      , DF.optIndexes = [0 .. length animalNames :: Int]
      }

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
indexTest = DF.index df1 == [0, 1]

rd :: IO ()
rd = putStr rendered

displayDf1 :: IO ()
displayDf1 = DF.display df1

flagged :: DataFrame Int (PersonFields .+ "flagged" .== Bool)
flagged = DF.extend #flagged False df1

plusFull :: Rec PersonFields
         -> Rec (PersonFields .+ "fullName" .== Text)
plusFull r = Rec.extend #fullName fname r
  where
    fname = r .! #name

capitalize :: DataFrame Int PersonFields
           -> DataFrame Int (PersonFields .+ "capsName" .== Text)
capitalize = DF.extendWith #capsName (\r -> Text.toUpper $ r .! #name)
