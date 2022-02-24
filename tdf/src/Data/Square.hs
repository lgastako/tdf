{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Square
  ( module Control.Applicative
  , module Data.Foldable
  , module Data.Functor
  , module Data.Traversable
  , Square
  -- Constructors
  , empty
  , fromList
  , fromSizedVectors
  -- Combinators
  , transpose
  , zipWith
  , zip
  -- Eliminators
  , toShows
  , unSquare
  ) where

import Data.Grid.Prelude hiding ( empty
                                , transpose
                                , zip
                                , zipWith
                                )

import qualified Control.Applicative
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Traversable

import qualified Data.List           as L
import qualified Data.Vector.Sized.X as SV

newtype Square (r :: Nat)  (c :: Nat) a = Square
  (SV.Vector c (SV.Vector r a))
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance (KnownNat r, KnownNat c) => Applicative (Square r c) where
  pure = Square . pure . pure
  Square f <*> Square x = Square $ SV.zipWith (SV.zipWith ($)) f x

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: Square 0 0 a
empty = Square SV.empty

fromList :: forall r c a.
            ( KnownNat r
            , KnownNat c
            )
         => [[a]]
         -> Maybe (Square r c a)
fromList xs
  | nSizes == 1 = Just (unsafeFromList xs)
  | otherwise   = Nothing
  where
    nSizes :: Int
    nSizes = length . L.nub . map length $ xs

unsafeFromList :: forall r c a.
                  ( KnownNat r
                  , KnownNat c
                  )
               => [[a]]
               -> Square r c a
unsafeFromList = map SV.unsafeFromList
  >>> SV.unsafeFromList
  >>> SV.transpose
  >>> Square

fromSizedVectors :: SV.Vector c (SV.Vector r a) -> Square r c a
fromSizedVectors = Square

-- ================================================================ --
--   Combinators
-- ================================================================ --

transpose :: forall r c a.
             ( KnownNat r
             , KnownNat c
             )
          => Square r c a
          -> Square c r a
transpose = unSquare >>> SV.transpose >>> Square

zipWith :: forall r c a b z.
           ( KnownNat r
           , KnownNat c
           )
        => (a -> b -> z)
        -> Square r c a
        -> Square r c b
        -> Square r c z
zipWith f a b = f <$> a <*> b

zip :: forall r c a b.
       ( KnownNat r
       , KnownNat c
       )
    => Square r c a
    -> Square r c b
    -> Square r c (a, b)
zip = zipWith (,)

-- ================================================================ --
--   Eliminators
-- ================================================================ --

toShows :: forall r c a.
           Show a
        => Square r c a
        -> [[Text]]
toShows = unSquare
  >>> map SV.toList
  >>> SV.toList
  >>> map (map show)

unSquare :: Square r c a -> SV.Vector c (SV.Vector r a)
unSquare (Square crv) = crv
