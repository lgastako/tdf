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

module TDF.CSV
  ( Error(..)
  , fromHeadedCSV
  , toHeadedCSV
  ) where

import TDF.Prelude

import qualified Data.List         as L
import qualified Data.Row.Records  as Rec
import           Data.String              ( String )
import qualified Data.Text         as T
import           TDF.DataFrame            ( DataFrame
                                          , Something
                                          )
import qualified TDF.DataFrame     as DF
import           TDF.Types.FromField      ( FromField( fromField ) )
import           TDF.Types.ToField        ( ToField( toField ) )

data Error
  = FileNotFound FilePath
  | FromCSVError String
  deriving (Eq, Generic, Ord, Show)

-- TODO: catch and return FileNotFound
fromHeadedCSV :: ( AllUniqueLabels a
                 , Forall a FromField
                 , Forall a Unconstrained1
                 , SNatI n
                 )
              => FilePath
              -> IO (Either Error (Maybe (DataFrame n Int a)))
fromHeadedCSV path = (recFromCSV <$> readFile path)
  <&> either (Left . FromCSVError)
             (Right . DF.fromList)

toHeadedCSV :: ( AllUniqueLabels (Map (Vec n) a)
               , Forall a ToField
               , Forall (Map (Vec n) a) Something
               , Forall a Unconstrained1
               , SNatI n
               )
            => FilePath
            -> DataFrame n Int a
            -> IO ()
toHeadedCSV path = writeFile path . recToCSV . DF.toList

recToCSV :: forall ρ. Forall ρ ToField => [Rec ρ] -> Text
recToCSV rs = T.unlines $ map (T.intercalate ",")
  $ Rec.labels @ρ @ToField
  : map (Rec.erase @ToField toField) rs

recFromCSV :: forall ρ.
              ( AllUniqueLabels ρ
              , Forall ρ FromField
              )
           => Text
           -> Either String [Rec ρ]
recFromCSV s = case map (T.splitOn ",") (T.lines s) of
  [] -> Left "No Input"
  header:vals -> traverse makeRecord vals
    where
      makeRecord s' = Rec.fromLabelsA @FromField @(Either String) @ρ (makeField s')

      makeField :: ( KnownSymbol l
                   , FromField a
                   )
                => [Text]
                -> Label l
                -> Either String a
      makeField val l = maybe
        (Left $ "Missing field " ++ (sid . show) l)
        fromField
        $ L.lookup (T.pack $ show l) (zip header val)
        where
          sid :: Text -> String
          sid = show
