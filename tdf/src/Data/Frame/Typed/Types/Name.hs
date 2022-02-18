{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Frame.Typed.Types.Name
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

import Test.QuickCheck     ( Arbitrary( arbitrary ) )
import Data.Text.Arbitrary ()

import qualified Data.Text as T

newtype Name = Name { unName :: Text }
  deriving (Eq, Generic, Ord, Semigroup, Show)

data Error = TrimmedNameIsEmpty
  deriving (Eq, Generic, Ord, Show)

instance Exception Error

instance Arbitrary Name where
  arbitrary = maybe arbitrary pure =<< fromText <$> arbitrary

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
           , Functor f
           , Semigroup (f Name)
           )
        => f Name
        -> f Name
        -> f Name
combine a b = a <> pure (Name " ") <> b
