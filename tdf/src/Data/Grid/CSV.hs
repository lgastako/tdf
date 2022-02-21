{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Grid.CSV
  ( fromCSV
  ) where

import Data.Grid.Prelude

import Data.Grid.Frame  ( Frame )
import Data.Grid.Series ( Series )
-- import qualified Data.Grid.Frame as F

import qualified Data.Vector.Sized as Sized

data Error
  = FileNotFound FilePath
  deriving (Eq, Generic, Ord, Show)

instance Exception Error

fromCSV :: MonadIO m
        => FilePath
        -> m (Either Error (Frame c r ci ri a))
fromCSV path = do
  _contents <- liftIO $ readFile path

  undefined

parse :: Text -> Either Error (Sized.Vector n k (Series m j a))
parse = undefined
