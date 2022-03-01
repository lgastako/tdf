{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.DeleteMe where

import Data.Frame.Prelude

import Data.Frame.Typed        ( Frame )
import Data.Frame.Typed.Series ( Series )

import qualified Data.Frame.Typed        as DF
import qualified Data.Frame.Typed.Series as Series

twoDigitCubes :: [(Int, Int)]
twoDigitCubes = map f [10..99]
  where
    f = (,) <*> (\x -> x * x * x)

series :: Series (Mult Nat9 Nat10) Int Int
series = twoDigitCubes
  |> (   map snd
     >>> Series.fromList
     >>> fromMaybe error
     )
  where
    error = panic "boom sha laka laka"

df :: Frame (Mult Nat9 Nat10) Int ("value" .== Int)
df = DF.fromSeries series

bf :: Frame Nat9 Int ("actual" .== Double .+ "expected" .== Double)
bf = DF.benford #value df
