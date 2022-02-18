{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Frame.Typed.Examples.Prestige where

import Data.Frame.Prelude
import Data.Frame.Typed   ( Frame )
import Data.Type.Nat
-- import Data.Nat

import qualified Data.Frame.Typed     as DF
import qualified Data.Frame.Typed.CSV as CSV

type Prestige = "id"        .== Text
             .+ "education" .== Float
             .+ "income"    .== Float
             .+ "women"     .== Float
             .+ "prestige"  .== Float
             .+ "census"    .== Float
             .+ "type_"     .== Text

-- "id","education","income","women","prestige","census","type_"
-- "gov.administrators",13.11,12351,11.16,68.8,1113,"prof"

-- % wc -l data/Prestige.csv                                                                                                                             ✹
--      103 data/Prestige.csv

-- We need the size not counting the header, so 102...

-- type PrestigeLen = Nat1

type PrestigeLen = Plus
  (Mult Nat3 Nat7)
  (Mult Nat9 Nat9)

thePrestige :: IO ()
thePrestige = do
  let n :: Nat
      n = reify (snatToNat $ snat @PrestigeLen) $ reflect
  print ("PrestigeLen", n)
  df :: Frame PrestigeLen Int Prestige
    <- CSV.unsafeFromHeadedCSV "data/prestige.csv"
  print . DF.columnNames $ df
  print . DF.toTexts $ df
  DF.display . DF.head @Nat5 $ df
