{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- mostly ganked from https://target.github.io/row-types/examples/RowCSV.html

module TDF.DataFrame.CSV
  ( Error(..)
  , fromHeadedCSV
  -- temp
  , fromCSV
  , toCSV
  ) where

import TDF.Prelude

import qualified Data.List         as L
import           Data.Row
import qualified Data.Row.Records  as Rec
import           Data.String              ( String )
import qualified Data.Text         as T
-- import qualified Data.Text.IO      as T
import           TDF.DataFrame            ( DataFrame )
import qualified TDF.DataFrame     as DF
import           TDF.Types.FromField      ( FromField( fromField ) )
import           TDF.Types.ToField        ( ToField( toField ) )

data Error
  = FileNotFound FilePath

fromHeadedCSV :: FilePath -> IO (Either Error (DataFrame Int a))
fromHeadedCSV _path = panic "fromHeadedCSV"
  where
    _ = panic "soon" :: DF.Axes idx

-- ================================================================ --

data PL = PL
  { name   :: Text
  , year   :: Int
  , person :: Text
  } deriving (Eq, Ord, Show, Generic)

pls :: [PL]
pls =
    [ PL "Haskell" 1990 "Simon"
    , PL "Scala"   2004 "Martin"
    , PL "Idris"   2009 "Edwin"
    , PL "Perl"    1987 "Larry"
    ]

input :: Text
input = T.unlines
    [ "year,name,types,person,website"
    , "1987,Perl,no,Larry"
    , "1990,Haskell,nice,Simon,https://www.haskell.org/"
    , "2004,Scala,weird,Martin,https://www.scala-lang.org/"
    , "2009,Idris,fancy,Edwin,https://www.idris-lang.org/"
    ]

recToCSV :: forall ρ. Forall ρ ToField => [Rec ρ] -> Text
recToCSV rs = T.unlines $ map (T.intercalate ",")
  $ Rec.labels @ρ @ToField
  : map (Rec.erase @ToField toField) rs

toCSV :: forall ρ t.
         ( Rec.FromNative t
         , Rec.NativeRow t ≈ ρ
         , Forall ρ ToField
         )
      => [t]
      -> Text
toCSV = recToCSV @ρ . fmap Rec.fromNative

_sanity1 :: IO ()
_sanity1 = putStr $ toCSV pls

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

      makeField :: (KnownSymbol l, FromField a) => [Text] -> Label l -> Either String a
      makeField val l = maybe (Left $ "Missing field " ++ show l) fromField
        $ L.lookup (T.pack $ show l) (zip header val)

fromCSV :: forall t ρ.
           ( Rec.ToNative t
           , ρ ≈ Rec.NativeRow t
           , AllUniqueLabels ρ
           , Forall ρ FromField
           )
        => Text
        -> Either String [t]
fromCSV = fmap (fmap Rec.toNative) . recFromCSV @ρ

_sanity2 :: IO ()
_sanity2 = case fromCSV @PL input of
  Left err -> putStrLn $ "ERROR: " ++ err
  Right xs -> mapM_ print xs
