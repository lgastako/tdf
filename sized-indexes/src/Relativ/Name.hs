{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module Relativ.Name
  ( Name
  , Error(..)
  , fromText
  , fromTextE
  , unName
  ) where

import Relativ.Prelude

import qualified Data.Text as T

newtype Name = Name Text
  deriving (Eq, Ord, Show)

data Error
  = EmptyName
  | TooLong
  deriving (Eq, Ord, Show)

instance Exception Error

fromTextE :: Text -> Either Error Name
fromTextE (T.strip -> s)
  | T.null s            = Left EmptyName
  | T.length s > maxLen = Left TooLong
  | otherwise           = Right (Name s)
  where
    maxLen = 255

fromText :: Text -> Maybe Name
fromText = hush . fromTextE

unName :: Name -> Text
unName (Name n) = n
