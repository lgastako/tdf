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

import           Data.Frame.Prelude

import qualified Data.List                        as L
import qualified Data.Row.Records                 as Rec
import           Data.String                             ( String )
import qualified Data.Text                        as T
import           Data.Frame.Typed                        ( -- AFrame
                                                             -- ,
                                                           Frame
                                                         )
import qualified Data.Frame.Typed                 as DF
import           Data.Frame.Typed.Types.FromField        ( FromField( fromField ) )
import           Data.Frame.Typed.Types.ToField          ( ToField( toField ) )

data Error
  = FileNotFound FilePath
  | FromCSVError String
  deriving (Eq, Generic, Ord, Show)

-- afromHeadedCSV :: ( AllUniqueLabels a
--                   , Forall a FromField
--                   , Forall a Unconstrained1
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
fromHeadedCSV :: ( AllUniqueLabels a
                 , Forall a FromField
                 , Forall a Unconstrained1
                 , SNatI n
                 )
              => FilePath
              -> IO (Either Error (Maybe (Frame n Int a)))
fromHeadedCSV path = (recFromCSV <$> readFile path)
  <&> either (Left . FromCSVError)
             (Right . DF.fromList)

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

      makeField :: ( KnownSymbol k
                   , FromField a
                   )
                => [Text]
                -> Label k
                -> Either String a
      makeField val k = maybe
        (Left $ "Missing field " ++ (sid . show) k
                   ++ " header=" ++ show header)
        fromField
        $ L.lookup (T.pack $ show k) (zip header val)
        where
          sid :: Text -> String
          sid = show

toHeadedCSV :: ( AllUniqueLabels (Map (Vec n) a)
               , Forall a ToField
               , Forall (Map (Vec n) a) Unconstrained1
               , Forall a Unconstrained1
               , SNatI n
               )
            => FilePath
            -> Frame n Int a
            -> IO ()
toHeadedCSV path = writeFile path . recToCSV . DF.toList

unsafeFromHeadedCSV :: forall n a.
                       ( AllUniqueLabels a
                       , Forall a FromField
                       , Forall a Unconstrained1
                       , SNatI n
                       )
                    => FilePath
                    -> IO (Frame n Int a)
unsafeFromHeadedCSV path = fromHeadedCSV path >>= \case
  Left error     -> panic (show error)
  Right Nothing  -> panic "unsafeFromHeadedCSV: Nothing"
  Right (Just x) -> pure x
