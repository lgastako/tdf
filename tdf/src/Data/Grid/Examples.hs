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

-- import qualified Data.Grid.Frame  as F
-- import qualified Data.Grid.Index  as I
import qualified Data.Grid.Series as S

readNBA :: IO (Frame 9 9 Int Int Text)
readNBA = fromCSV "data/nba9.csv" <&> either (panic . show) identity

-- The file only has 9 tottal - 8 + header... but oh, we're not treating headers as keys yet...
-- so the header row only gets treated as a header when it's rendered... it's all text botherwise.
-- TODO: This only loads 8 rows.. it's not properly accounting for the headers
--       or something... but that doesn't make sense since there the type has 9
--       rows...
example1 :: IO ()
example1 = putText . f =<< readNBA
  where
    f = render :: Frame 9 9 Int Int Text -> Text

example2 :: IO ()
example2 = case S.fromList @5 @Int [1..5 :: Int]  of
  Nothing -> panic "example2 boom"
  Just s  -> S.display s

example3 :: IO ()
example3 = do
  df <- readNBA
  -- TODO: reset indexes of rows to [1..]
  -- TODO: reset indexes of cols to first row headers
  -- TODO: drop to (age, weight, number columns)
  -- TODO: map readMaybe, then sequenceA to get Maybe (Series (Read a))
  undefined
