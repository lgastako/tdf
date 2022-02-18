{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Frame.Typed.Types.Widths
  ( Widths( unWidths )
  , fill
  , pad
  , total
  , widths
  ) where

import           Data.Frame.Prelude

import qualified Data.List          as List
import qualified Data.Text          as Text

newtype Widths = Widths { unWidths :: [Int] }
  deriving (Eq, Generic, Ord, Show)

fill :: Widths -> Text -> [Text]
fill (Widths ws) s = map (`Text.replicate` s) ws

pad :: Widths -> [Text] -> [Text]
pad = zipWith padN . unWidths

padN :: Int -> Text -> Text
padN n s = Text.replicate (n - Text.length s) " " <> s

total :: Widths -> Int
total = sum . unWidths

widths :: [[Text]] -> Widths
widths = Widths . map (maximum . map Text.length) . List.transpose
