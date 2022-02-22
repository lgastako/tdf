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

import qualified Data.Text as T

newtype Name = Name Text
  deriving (Eq, Generic, Ord, Show)

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

combine :: forall f.
           ( Applicative f
           , Foldable f
           , Monoid (f Name)
           )
        => f Name
        -> f Name
        -> f Name
combine a b = fold
  [ pure (Name "[")
  , a
  , pure (Name " >> ")
  , b
  , pure (Name "]")
  ]

-- ================================================================ --
--   Eliminators
-- ================================================================ --

unName :: Name -> Text
unName (Name n) = n

-- ================================================================ --
--   Helpers/Other
-- ================================================================ --

maxNameLen :: Int
maxNameLen = 2 ^ (8 :: Int)

normalize :: Text -> Text
normalize = T.strip
