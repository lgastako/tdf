{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TDF.Types.Widths
  ( Widths( unWidths )
  , pad
  , widths
  ) where

import           TDF.Prelude

import qualified Data.List   as List
import qualified Data.Text   as Text

newtype Widths = Widths { unWidths :: [Int] }
  deriving (Eq, Generic, Ord, Show)

widths :: [[Text]] -> Widths
widths = Widths . map (maximum . map Text.length) . List.transpose

pad :: Widths -> [Text] -> [Text]
pad (Widths w) = zipWith padN w
  where

padN :: Int -> Text -> Text
padN n s = s <> (Text.replicate (n - Text.length s) " ")
