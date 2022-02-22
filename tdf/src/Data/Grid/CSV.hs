{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Grid.CSV
  ( Error(..)
  , fromCSV
  ) where

import Data.Grid.Prelude

import Data.Grid.Frame  ( Frame )
import Data.Grid.Series ( Series )

import qualified Data.Grid.Frame   as F
import qualified Data.Grid.Series  as S
import qualified Data.Text         as T

data Error
  = FileNotFound FilePath
  | EmptyFile FilePath
  deriving (Eq, Generic, Ord, Show)

instance Exception Error

fromCSV :: forall c r ci ri m a.
           ( KnownNat c
           , KnownNat r
           , Enum ci --TODO check
           , Enum ri --TODO check
           , MonadIO m
           , a ~ Text  -- for now
           )
        => FilePath
        -> m (Either Error (Frame c r ci ri a))
fromCSV path = liftIO $
  ((Right <$> readFile path) `catch` (pure . Left)) >>= \case
    Left error -> pure $ Left error
    Right (textToTexts -> rows) -> do
      let columns :: [[Text]]
          columns = transpose rows

          rowMaySeries :: [Maybe (Series r ri Text)]
          rowMaySeries = map S.fromList columns

          rowSeriesMay :: Maybe [Series r ri Text]
          rowSeriesMay = sequenceA rowMaySeries

          rowSeries :: [Series r ri Text]
          rowSeries = case rowSeriesMay of
            Nothing -> panic "fix me"
            Just rs -> rs

          colSeriesMay :: Maybe (Series c ci (Series r ri Text))
          colSeriesMay = S.fromList rowSeries

          colSeries :: Series c ci (Series r ri Text)
          colSeries = case colSeriesMay of
            Nothing -> panic "fixme too"
            Just c' -> c'

          frame :: Frame c r ci ri a
          frame = F.fromSeries colSeries
      pure $ Right frame

textToTexts :: Text -> [[Text]]
textToTexts = map (T.splitOn ",") . T.lines
