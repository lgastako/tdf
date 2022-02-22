{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Grid.Name
  ( Name
  -- Constructors
  , fromText
  , fromTextE
  , unsafeFromText
  -- Combinators
  , combine
  -- Eliminators
  , unName
  -- Helpers
  , normalize
  ) where

import Data.Grid.Prelude

import qualified Data.List.NonEmpty      as NE
import qualified Data.Semigroup.Foldable as SF
import qualified Data.Text               as T

newtype Name = Name Text
  deriving (Eq, Generic, Ord, Show)

instance Semigroup Name where
  Name a <> Name b = Name (a <> b)


data Error
  = NameTooLong
  | NormalizedTextWasBlank
  deriving (Eq, Generic, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromText :: Text -> Maybe Name
fromText = hush . fromTextE

fromTextE :: Text -> Either Error Name
fromTextE (normalize -> s)
  | T.null s                = Left NormalizedTextWasBlank
  | T.length s > maxNameLen = Left NameTooLong
  | otherwise               = Right . Name $ s

unsafeFromText :: Text -> Name
unsafeFromText (normalize -> s) = Name s

-- ================================================================ --
--   Combinators
-- ================================================================ --

combine :: Name
        -> Name
        -> Name
combine a b = SF.fold1 . NE.fromList $
  [ Name "["
  , a
  , Name " >> "
  , b
  , Name "]"
  ]

-- ================================================================ --
--   Eliminators
-- ================================================================ --

unName :: Name -> Text
unName (Name n) = n

-- ================================================================ --
--   Helpers
-- ================================================================ --

maxNameLen :: Int
maxNameLen = 2 ^ (8 :: Int)

normalize :: Text -> Text
normalize = T.strip