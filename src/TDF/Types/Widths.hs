{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Types.Widths
  ( Widths( unWidths )
  , fill
  , pad
  , total
  , widths
  ) where

import           TDF.Prelude

import qualified Data.List   as List
import qualified Data.Text   as Text

newtype Widths = Widths { unWidths :: [Int] }
  deriving (Eq, Generic, Ord, Show)

fill :: Widths -> Text -> [Text]
fill (Widths ws) s = map (flip Text.replicate s) ws

pad :: Widths -> [Text] -> [Text]
pad = zipWith padN . unWidths

padN :: Int -> Text -> Text
padN n s = s <> (Text.replicate (n - Text.length s) " ")

total :: Widths -> Int
total = sum . unWidths

widths :: [[Text]] -> Widths
widths = Widths . map (maximum . map Text.length) . List.transpose
