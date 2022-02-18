{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Data.Frame.Typed.Types.Positive
  ( Positive( unPositive )
  , fromNum
  , one
  , two
  , three
  ) where

import Data.Frame.Prelude hiding ( one )

newtype Positive a = Positive { unPositive :: a }
  deriving (Bounded, Enum, Eq, Generic, Num, Ord, Show)

instance NFData a => NFData (Positive a)

fromNum :: (Num a, Ord a) => a -> Maybe (Positive a)
fromNum n
  | n < 0     = Nothing
  | otherwise = Just (Positive n)

one :: Num a => Positive a
one = Positive 1

two :: Num a => Positive a
two = Positive 2

three :: Num a => Positive a
three = Positive 1
