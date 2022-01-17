{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
module Super.Examples.TP where

import           Data.Vector           ( Vector )
import           Super.DataFrame       ( DataFrame )
import qualified Super.DataFrame as DF
import           SuperRecord           ( (&)
                                       , (:=)(..)
                                       , (:=)
                                       , Rec
                                       , rnil
                                       )

-- https://www.tutorialspoint.com/python_pandas/python_pandas_dataframe.htm

-- TODO: Damnit SuperRecord, why does the order of fields change whether
-- it compiles or not??
type StudentFields =
  [ "marksPct" := Float
  , "name"     := String
  , "regNo"    := Int
  ]

type StudentRec = Rec StudentFields

mkStudent :: Int -> String -> Float -> StudentRec
mkStudent r n m = #name := n
                & #marksPct := m
                & #regNo := r
                & rnil

exampleList1 :: [StudentRec]
exampleList1 =
  [ mkStudent 1000 "Steve"  86.29
  , mkStudent 1001 "Mathew" 91.63
  , mkStudent 1002 "Jose"   72.90
  , mkStudent 1003 "Patty"  69.23
  , mkStudent 1004 "Vin"    88.30
  ]

exampleDf :: DataFrame StudentFields
exampleDf = DF.fromList exampleList1

-- ================================================================
--     Example 1
-- ================================================================

-- import pandas as pd
-- df = pd.DataFrame()
-- print df
-- Its output is as follows −

-- Empty DataFrame
-- Columns: []
-- Index: []

-- Haskell equivalent:

example1Df :: DataFrame '[]
example1Df = DF.empty

example1Vec :: Vector (Rec '[])
example1Vec = DF.toVector example1Df

-- >>> DF.toVector DF.empty
-- []

-- TODO: make a way to get the output that python's print gives with the
-- columns and indexes

-- TODO we also need to store indexes, etc. let's get to that sooner rather than later.
-- probably we will need to create a "construct" function that takes options or something.

-- ================================================================
--     Example 2
-- ================================================================

-- import pandas as pd
-- data = [1,2,3,4,5]
-- df = pd.DataFrame(data)
-- print df
-- Its output is as follows −

--      0
-- 0    1
-- 1    2
-- 2    3
-- 3    4
-- 4    5

-- Haskell:

-- >>> df = DF.fromList [1..5]
-- >>> DF.toVector df
-- TODO

