{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TDF.Table
  ( Table
  , display
  , from
  , fromRows
  , fromHeadedRows
  , render
  ) where

import           TDF.Prelude       hiding ( from )

import qualified Data.List as List
import qualified Data.Text as Text

newtype Table = Table (Maybe Header, [Row])
  deriving (Eq, Generic, Ord, Show)

newtype Header = Header Row
  deriving (Eq, Generic, Ord, Show)

newtype Row = Row {unRow :: [Text]}
  deriving (Eq, Generic, Ord, Show)

newtype Widths = Widths { unWidths :: [Int] }
  deriving (Eq, Generic, Ord, Show)

display :: Table -> IO ()
display = putStr . render

from :: Maybe Header -> [Row] -> Table
from = curry Table

fromHeadedRows :: [Row] -> Maybe Table
fromHeadedRows (header:rows) = Just (from (Just $ Header header) rows)
fromHeadedRows []            = Nothing

fromRows :: [Row] -> Table
fromRows = from Nothing

_example :: [Row]
_example = map Row
  [ ["name", "age"]
  , ["John", "46"]
  , ["Dave", "52"]
  ]

render :: Table -> Text
render = \case
  Table (Nothing,  rows) -> renderBody rows
  Table (Just hdr@(Header hRow), rows) -> Text.concat
    [ renderHeader (widths (hRow:rows)) hdr
    , renderBody rows
    ]

widths :: [Row] -> Widths
widths = Widths . map (maximum . map Text.length) . List.transpose . map unRow

renderBody :: [Row] -> Text
renderBody rows@(map unRow -> texts) = undefined
  where
    _ = texts :: [[Text]]
    paddedTexts = map pad texts

    ws :: Widths
    ws = widths rows

    pad :: [Text] -> [Text]
    pad = zipWith p ws

    p :: Int -> Text -> Text
    p = undefined

renderHeader :: Widths -> Header -> Text
renderHeader widths (Header row) = undefined
