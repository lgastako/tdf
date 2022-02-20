{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- mostly ganked from https://target.github.io/row-types/examples/RowCSV.html

module Data.Frame.Typed.CSV
  ( Error(..)
  , fromHeadedCSV
  , toHeadedCSV
  , unsafeFromHeadedCSV
  ) where

import Data.Frame.Prelude

import Data.String                ( String )
import Data.Frame.Typed           ( Frame  )
import Data.Frame.Typed.FromField ( FromField( fromField ) )
import Data.Frame.Typed.ToField   ( ToField( toField ) )

import qualified Data.List        as L
import qualified Data.Row.Records as Rec
import qualified Data.Text        as T
import qualified Data.Frame.Typed as DF

data Error
  = FileNotFound FilePath
  | FromCSVError String
  deriving (Eq, Generic, Ord, Show)

-- afromHeadedCSV :: ( Forall a FromField
--                   , WellBehaved a
--                   )
--               => FilePath
--               -> IO (Either Error (AFrame Int a))
-- afromHeadedCSV path = do
--   contents <- readFile path
--   case recFromCSV contents of
--     Left  error -> pure . Left . FromCSVError $ error
--     Right val   -> do
--       let --  Just xs = DF.fromList val
--           xs = undefined
--           _ = val :: [Rec a]
--       pure . Right $ DF.AFrame undefined xs
--   -- (recFromCSV <$> readFile path)
--   -- <&> either (Left . FromCSVError)
--   --            (Right . DF.fromList)

-- TODO: catch and return FileNotFound
fromHeadedCSV :: ( Forall a FromField
                 , SNatI n
                 , WellBehaved a
                 )
              => FilePath
              -> IO (Either Error (Maybe (Frame n Int a)))
fromHeadedCSV path = (recFromCSV <$> readFile path)
  <&> either (Left . FromCSVError)
             (Right . DF.fromList)

recToCSV :: forall ρ.
            ( Forall ρ ToField )
         => [Rec ρ]
         -> Text
recToCSV rs = T.unlines $ map (T.intercalate ",")
  $ Rec.labels @ρ @ToField
  : map (Rec.erase @ToField toField) rs

recFromCSV :: forall a.
              ( AllUniqueLabels a
              , Forall a FromField
              )
           => Text
           -> Either String [Rec a]
recFromCSV s = case map (T.splitOn ",") (T.lines s) of
  [] -> Left "No Input"
  header:vals -> traverse makeRecord vals
    where
      makeRecord s' =
        Rec.fromLabelsA @FromField @(Either String) @a (makeField s')

      makeField :: ( KnownSymbol k
                   , FromField b
                   )
                => [Text]
                -> Label k
                -> Either String b
      makeField val k = maybe
        (Left $ "Missing field " ++ (sid . show) k
                   ++ " header=" ++ show header)
        fromField
        $ L.lookup (T.pack $ show k) (zip header val)
        where
          sid :: Text -> String
          sid = show

toHeadedCSV :: forall n a.
               ( Forall a ToField
               , Forall a Unconstrained1
               , SNatI n
               , WellBehaved (Map (Vec n) a)
               )
            => FilePath
            -> Frame n Int a
            -> IO ()
toHeadedCSV path = writeFile path . recToCSV . DF.toList

unsafeFromHeadedCSV :: forall n a.
                       ( Forall a FromField
                       , SNatI n
                       , WellBehaved a
                       )
                    => FilePath
                    -> IO (Frame n Int a)
unsafeFromHeadedCSV path = fromHeadedCSV path >>= \case
  Left error     -> panic (show error)
  Right Nothing  -> panic "unsafeFromHeadedCSV: Nothing"
  Right (Just x) -> pure x
