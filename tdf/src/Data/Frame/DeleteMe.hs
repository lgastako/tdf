{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Frame.DeleteMe where

import Data.Frame.Prelude

import qualified Data.Csv             as CSV
import qualified Data.Vector          as V
import qualified Data.ByteString.Lazy as BL

type Row = ( Text
           , Float
           , Float
           , Float
           , Float
           , Float
           , Text
           )

test :: IO (V.Vector Row)
test = (CSV.decode CSV.HasHeader <$> BL.readFile path) >>= \case
  Left err -> panic (cs err)
  Right v  -> traverse f v
  where
    path = "data/Prestige.csv"

    f :: Row -> IO Row
    f row = do
--      print row
      pure row
