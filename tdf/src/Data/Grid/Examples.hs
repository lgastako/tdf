{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Grid.Examples where

import Data.Grid.Prelude

import Data.Grid.Renderable ( Renderable( render ) )
import Data.Grid.CSV        ( fromCSV )
import Data.Grid.Frame      ( Frame )

-- import qualified Data.Grid.CSV as CSV

-- import qualified Data.Grid.Frame  as F
-- import qualified Data.Grid.Index  as I
import qualified Data.Grid.Series as S

-- TODO: This only loads 8 rows.. it's not properly accounting for the headers
--       or something... but that doesn't make sense since there the type has 9
--       rows...
example1 :: IO ()
example1 = fromCSV "data/nba9.csv" >>= putText . either show toResult
  where
    toResult = render :: Frame 9 9 Int Int Text -> Text

example2 :: IO ()
example2 = case S.fromList @5 @Int [1..5 :: Int]  of
  Nothing -> panic "example2 boom"
  Just s  -> S.display s
