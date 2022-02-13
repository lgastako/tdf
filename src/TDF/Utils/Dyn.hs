{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.Utils.Dyn
  ( fromAnyDyn
  , getValue
  ) where

import           TDF.Prelude              hiding ( empty
                                                 , foldr
                                                 , head
                                                 , map
                                                 , toList
                                                 )

import           Data.Dynamic                    ( Dynamic
                                                 , fromDynamic
                                                 )
import           Data.HashMap.Strict             ( HashMap )
import qualified Data.HashMap.Strict  as HashMap
import           Data.String                     ( String )

-- TODO I tried to do something like this:
--
--   case fromAnyDyn dyn of
--     Just (x :: forall a. ToField a => a) -> ...
--
-- but I couldn't get it working.  TODO: Ask on Haskell Slack.
fromAnyDyn :: Dynamic -> Maybe Text
fromAnyDyn dyn = case fromDynamic dyn of
  Just (t :: Text) -> Just t
  Nothing -> case fromDynamic dyn of
    Just (d :: Double) -> Just . show $ d
    Nothing -> case fromDynamic dyn of
      Just (i :: Integer) -> Just . show $ i
      Nothing -> case fromDynamic dyn of
        Just (i :: Int) -> Just . show $ i
        Nothing -> case fromDynamic dyn of
          Just (f :: Float) -> Just . show $ f
          Nothing -> case fromDynamic dyn of
            Just (s :: String) -> Just . cs $ s
            Nothing -> Nothing

getValue :: Text -> HashMap Text Dynamic -> Text
getValue k m = maybe noKeyError f $ HashMap.lookup k m
  where
    f v = fromAnyDyn v & fromMaybe noDynError

    noKeyError = "getValue failed: key not found in map: " <> k
    noDynError = "getValue failed: no value undynamicable"
