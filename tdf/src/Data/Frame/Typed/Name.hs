{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Frame.Typed.Name
  ( Error(..)
  , Name( unName )
  -- Value
  , series
  -- Constructors
  , fromText
  , fromTextE
  , unsafeFromText
  -- Combinators
  , combine
  ) where

import Data.Frame.Prelude

import qualified Data.Text as T

newtype Name = Name { unName :: Text }
  deriving (Eq, Generic, Ord, Semigroup, Show)

data Error = TrimmedNameIsEmpty
  deriving (Eq, Generic, Ord, Show)

instance Exception Error

-- ================================================================ --
--   Values
-- ================================================================ --

series :: Name
series = unsafeFromText "series"

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromText :: Text -> Maybe Name
fromText = either (const Nothing) pure . fromTextE

fromTextE :: Text -> Either Error Name
fromTextE s = case T.strip s of
  ""   -> Left TrimmedNameIsEmpty
  name -> Right $ Name name

unsafeFromText :: Text -> Name
unsafeFromText s = case fromTextE s of
  Left error -> panic $ "Name.unsafeFromText: " <> show error
  Right name -> name

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
