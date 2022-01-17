{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Super.Examples where

import           Prelude        hiding ( drop
                                       , take
                                       )
import           SuperRecord           ( (&)
                                       , (:=)(..)
                                       , (:=)
                                       , Has
                                       , Rec
                                       , get
                                       , rnil
                                       )
import           Super.DataFrame       ( DataFrame )

import qualified Super.DataFrame as DF

type PersonFields =
  [ "age"  := Int
  , "name" := String
  ]

type Person = Rec PersonFields

type PlayerFields =
  [ "name" := String
  , "team" := String
  ]

type Player = Rec PlayerFields

type AgeFields = '["age" := Int]

type AgeRec = Rec AgeFields

type NameFields = '["name" := String]

type NameRec = Rec NameFields

personName1 :: NameRec
personName1 = #name := "John" & rnil

justName :: Person -> NameRec
justName p = #name := get #name p & rnil

person :: Person
person =
  #name := "Alex"
  & #age := 23
  & rnil

person2 :: Person
person2 =
  #age := 45
  & #name := "Dave"
  & rnil

greet :: Has "name" r String => Rec r -> String
greet = ("Hello " ++) . get #name

df1 :: DataFrame PersonFields
df1 = DF.fromList
  [ person
  , person2
  ]

df2 :: DataFrame NameFields
df2 = DF.map justName df1

df3 :: DataFrame NameFields
df3 = DF.column #name df1

df4 :: DataFrame AgeFields
df4 = DF.columnWith show #age df1

df5 :: DataFrame AgeFields
df5 = DF.map (DF.relabel' #age) df1
-- df5 = DF.column #age df1
-- df5 = DF.map (get #age) df1

rendered :: String
rendered = unlines
  [ DF.renderWith toStrings  df1
  , DF.renderWith toStrings' df2
  , DF.renderWith toStrings' df3
--   , DF.renderWith ageToString df4
  ]

ageToString :: Rec AgeFields -> [String]
ageToString = pure . show . get #age

toStrings :: Rec PersonFields -> [String]
toStrings rp =
  [ get #name rp
  , show (get #age rp)
  ]

toStrings' :: Rec NameFields -> [String]
toStrings' rp = [ get #name rp ]

r :: IO ()
r = putStr rendered
