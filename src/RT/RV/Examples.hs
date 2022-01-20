{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module RT.RV.Examples where

import           Prelude               hiding ( drop
                                              , take
                                              )

import           Control.Lens                 ( (^.)
                                              , view
                                              )
import           Data.Generics.Labels         ()
import           Data.Generics.Product.Fields ( HasField' )
import           Data.Row                     ( type (.+)
                                              , type (.==)
                                              ,      (.+)
                                              ,      (.==)
                                              , Rec
                                              )
import qualified Data.Row.Records as Rec
import           Data.Vector                  ( Vector)
import qualified Data.Vector      as Vector
import           GHC.Generics                 ( Generic )
import           RT.RV.DataFrame              ( DataFrame )
import qualified RT.RV.DataFrame  as DF

type PersonFields =
  (  "name" .== String
  .+ "age"  .== Int
  )

type VecPersonFields =
  (  "name" .== Vector String
  .+ "age"  .== Vector Int
  )

type    Person = Rec    PersonFields
type VecPerson = Rec VecPersonFields

type PlayerFields =
  (  "name" .== String
  .+ "team" .== String
  )

type Player = Rec PlayerFields

type AgeFields = "age" .== Int

type AgeRec = Rec AgeFields

type NameFields = "name" .== String

type NameRec = Rec NameFields

data Person' = Person'
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

personName1 :: NameRec
personName1 = #name .== "John"

justName :: Person -> NameRec
justName p = #name .== p ^. #name

person :: Person
person = #name .== "Alex"
      .+ #age .== 23

person2 :: Person
person2 = #age  .== 45
       .+ #name .== "Dave"

greet :: HasField' "name" a String => a -> String
greet = ("Hello " ++) . view #name

df1 :: DataFrame Int PersonFields
df1 = DF.fromList
  [ person
  , person2
  ]

df1' :: DataFrame Int PersonFields
df1' = DF.reindex [0, 1] df1

-- λ> DF.memSize df1'
-- 1096
-- Yeesh that's a lot of overhead for [{name:Alex,age:23},{name:Dave,age:45}]

df1'' :: DataFrame Int (PersonFields .+ "foo" .== String)
df1'' = DF.map (\x -> x .+ #foo .== ("bar" ::String)) df1'

df2 :: DataFrame Int NameFields
df2 = DF.map justName df1

-- df3' :: DataFrame Int NameFields
-- df3' = DF.map (\x -> ) df1

-- df3 :: DataFrame Int NameFields
-- df3 = DF.column #name df1

-- df4 :: DataFrame Int AgeFields
-- df4 = DF.columnWith f #age df1
--   where
--     f :: AgeRec -> AgeRec
--     f = id -- undefined -- show

-- df5 :: DataFrame Int AgeFields
-- df5 = DF.map (DF.relabel' #age) df1
-- -- df5 = DF.column #age df1
-- -- df5 = DF.map (get #age) df1

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

animals :: DataFrame Int ("animal" .== String)
animals = DF.construct o
  where
    o = DF.opts
      { DF.optData = Vector.fromList . map mkRec $ animalNames
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

rendered :: String
rendered = unlines
  [ DF.renderWith toStrings  df1
  , DF.renderWith toStrings' df2
--  , DF.renderWith toStrings' df3
--  , DF.renderWith ageToString df4
  ]

ageToString :: Rec AgeFields -> [String]
ageToString = pure . show . view #age

toStrings :: Rec PersonFields -> [String]
toStrings rp =
  [ rp ^. #name
  , show (rp ^. #age)
  ]

toStrings' :: Rec NameFields -> [String]
toStrings' rp = [ view #name rp ]

indexTest :: Bool
indexTest = DF.index df1 == [0, 1]

r :: IO ()
r = putStr rendered

-- dfFocused :: DataFrame Int NameFields
-- dfFocused = DF.map (Rec.focus #name) df1
