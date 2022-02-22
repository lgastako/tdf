{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
-- import qualified Data.Vector.Sized as Sized

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
fromCSV path = liftIO $ do
  (((Right <$> readFile path) `catch` (pure . Left))) >>= \case
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




--   let _ = contents :: Either Error Text

--       parseResult :: Either Error (Series n k Text)
--       parseResult = join . fmap parseLines $ contents

--       textAsSeries :: Either Error (Series n k (Series n' k Text))
--       textAsSeries = f <<$>> parseResult

--       f :: Text -> Series n' k Text
--       f = undefined -- g . S.fromList . T.splitOn ","

--       g = undefined

--       ss :: Series n k (Series n' k Text) -> Either Error (Frame c r ci ri a)
--       ss = F.fromSeries <$> textAsSeries

--   undefined
--   -- case parseLines contents of
--   --   Left error -> Left error
--   --   Right seriesOfLines -> undefined
--   where
--     handle1 :: Error -> IO (Either Error Text)
--     handle1 = pure . Left
-- -- (\() -> pure (Left (FileNotFound path))) -- const $ pure (Left FileNotFound))

-- parseLines :: Text -> Either Error (Series n k Text)
-- parseLines = undefined

-- --   let parsed :: Either Error (Sized.Vector n (Series n' k a))
-- --       parsed = parse path contents
-- --       result :: Either Error (Frame c r ci ri a)
-- --       result = pack <$> parsed
-- --   pure result

-- -- parse :: Text -> Series n k Text
-- -- parse = undefined

-- -- pack :: forall c r ci ri a.
-- --         ( Enum ci
-- --         , Enum ri
-- --         , KnownNat c
-- --         )
-- --      => Sized.Vector c (Series r ri a)
-- --      -> Frame c r ci ri a
-- -- pack vs = F.fromSeries ss
-- --   where
-- --     ss :: Series c ci (Series r ri a)
-- --     ss = S.fromVector vs

-- -- parse :: forall n m k a.
-- --          ( Enum k
-- --          , KnownNat m
-- --          )
-- --       => FilePath
-- --       -> Text
-- --       -> Either Error (Sized.Vector n (Series m k a))
-- -- parse path s = case map (T.splitOn ",") . T.lines $ s of
-- --   [] -> Left (EmptyFile path)
-- --   header:vals -> undefined
-- --     where
-- --       _ = header :: [Text]
-- --       _ = vals   :: [[Text]]

-- -- --      makeSeries :: [Text] -> Sized.Vector  (Series n k a)
-- --       makeSeries = undefined

-- -- --      foo :: Series m k [Series n k a]
-- --       foo = traverse makeSeries vals
