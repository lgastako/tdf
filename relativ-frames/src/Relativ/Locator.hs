{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Relativ.Locator
  ( Locator(..)
  ) where

import Relativ.Frame.Prelude

import Data.List.Split ( splitOn )
import Data.String     ( String )

import qualified Data.Vector as U

class Locator loc a where
  loc :: loc -> a -> a

instance {-# OVERLAPPING #-} Locator Int (U.Vector a) where
  loc m v = U.slice m (n - m) v
    where
      n = U.length v

-- TODO: should we handle m > n? or error? or what?
instance {-# OVERLAPPING #-} Locator (Int, Int) (U.Vector a) where
  loc (m, n) = U.slice m (n - m)

instance {-# OVERLAPPABLE #-} StringConv s String
  => Locator s (U.Vector a) where
  loc s v = case parse . cs $ s of
    Nothing -> case readMaybe str of
      Nothing -> panic "invalid loc string"
      Just m  -> loc (m :: Int) v
    Just (m, n) -> loc (m, n) v
    where
      parse :: String -> Maybe (Int, Int)
      parse s' = case splitOn ":" s' of
        [a, b] -> case readMaybe . cs $ a of
          Nothing -> Nothing
          Just a' -> case readMaybe . cs $ b of
            Nothing -> Nothing
            Just b' -> Just (a', b')
        _ -> Nothing

      str :: [Char]
      str = cs s

_passed :: Bool
_passed = and tests

tests :: [Bool]
tests =
  [ v1 == U.fromList [3, 4]
  , v2 == U.fromList [3..10]
  , v3 == v1
  , v4 == v2
  ]

v0 :: U.Vector Int
v0 = U.fromList [1..10]

l1 :: (Int, Int)
l1 = (2, 4)

v1 :: U.Vector Int
v1 = loc l1 v0

v2 :: U.Vector Int
v2 = loc (2 :: Int) v0

v3 :: U.Vector Int
v3 = loc ("2:4" :: Text) v0

v4 :: U.Vector Int
v4 = loc ("2" :: Text) v0
