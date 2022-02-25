{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Square
  ( module Control.Applicative
  , module Data.Foldable
  , module Data.Functor
  , module Data.Traversable
  , Square
  -- Constructors
  , empty
  , emptyCols
  , emptyRows
  , fromList
  , fromSizedVectors
  -- Optics
  , at
  -- Combinators
  , (<+>)
  , (<//>)
  , transpose
  , zipWith
  , zip
  -- Eliminators
  , toLists
  , toTexts
  , unSquare
  ) where

import Data.Grid.Prelude hiding ( empty
                                , transpose
                                , zip
                                , zipWith
                                )

import Data.Renderable          ( Renderable( render ) )
import Data.Vector.Sized.X      ( (!!)
                                , (//)
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

emptyCols :: KnownNat n => Square 0 n a
emptyCols = Square (pure SV.empty)

emptyRows :: Square n 0 a
emptyRows = Square SV.empty

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
--   Optics
-- ================================================================ --

at :: forall r c a.
      ( KnownNat r
      , KnownNat c
      )
   => ( Finite r
      , Finite c
      )
   -> Lens' (Square r c a) a
at (r, c) = lens
  (\(Square v) -> (v !! c) !! r)
  (\(Square v) x -> Square $ v // [(c, v !! c // [(r, x)])])

-- ================================================================ --
--   Combinators
-- ================================================================ --

(<//>) :: forall r ca cb a cc.
         ( KnownNat r
         , KnownNat ca
         , KnownNat cb
         , KnownNat cc
         , cc ~ (ca + cb)
         )
      => Square r ca a
      -> Square r cb a
      -> Square r cc a
a <//> b = transpose (transpose a <+> transpose b)

(<+>) :: forall ra rb rc c a.
         ( rc ~ (ra + rb) )
      => Square ra c a
      -> Square rb c a
      -> Square rc c a
Square a <+> Square b = Square $ SV.zipWith (SV.++) a b

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

toLists :: forall r c a.
           Square r c a
        -> [[a]]
toLists = unSquare
      >>> map SV.toList
      >>> SV.toList

toTexts :: forall r c a.
           Renderable a
        => Square r c a
        -> [[Text]]
toTexts = toLists
      >>> map (map render)

unSquare :: Square r c a -> SV.Vector c (SV.Vector r a)
unSquare (Square crv) = crv
