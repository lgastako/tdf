{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Rect
  ( module Control.Applicative
  , module Data.Foldable
  , module Data.Functor
  , module Data.Traversable
  , Rect
  , Square
  -- Constructors
  , empty
  , emptyCols
  , emptyRows
  , fromList
  , fromSizedVectors
  -- Optics
  , at
  , slice
  , sliceC
  , sliceR
  -- Combinators
  , (<+>)
  , (<//>)
  , transpose
  , zipWith
  , zip
  -- Eliminators
  , toLists
  , toTexts
  , unRect
  , unsafeFromList
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
-- import GHC.TypeLits             ( -- type (<=)
--                                 -- ,
--                                   type (-)
-- --                                , type (<=)
--                                 )

import qualified Control.Applicative
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Traversable

import qualified Data.List           as L
import qualified Data.Vector.Sized.X as SV

type Square (n :: Nat) = Rect n n

newtype Rect (r :: Nat)  (c :: Nat) a = Rect
  (SV.Vector c (SV.Vector r a))
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance (KnownNat r, KnownNat c) => Applicative (Rect r c) where
  pure = Rect . pure . pure
  Rect f <*> Rect x = Rect $ SV.zipWith (SV.zipWith ($)) f x

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: Rect 0 0 a
empty = Rect SV.empty

emptyCols :: KnownNat n => Rect 0 n a
emptyCols = Rect (pure SV.empty)

emptyRows :: Rect n 0 a
emptyRows = Rect SV.empty

fromList :: forall r c a.
            ( KnownNat r
            , KnownNat c
            )
         => [[a]]
         -> Maybe (Rect r c a)
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
               -> Rect r c a
unsafeFromList = map SV.unsafeFromList
  >>> SV.unsafeFromList
  >>> SV.transpose
  >>> Rect

fromSizedVectors :: SV.Vector c (SV.Vector r a) -> Rect r c a
fromSizedVectors = Rect

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
   -> Lens' (Rect r c a) a
at (r, c) = lens
  (\(Rect v) -> (v !! c) !! r)
  (\(Rect v) x -> Rect $ v // [(c, v !! c // [(r, x)])])

sliceC :: forall c' r c x m a p.
          ( KnownNat c'
          , KnownNat x
          , c ~ ((x + c') + m)
          )
       => p x
       -> Rect r c  a
       -> Rect r c' a
sliceC px (Rect v) = Rect (SV.slice px v)

sliceR :: forall r' r c y m a p.
          ( KnownNat r'
          , KnownNat y
          , r ~ ((y + r') + m)
          )
       => p y
       -> Rect r  c a
       -> Rect r' c a
sliceR py (Rect v) = Rect (SV.map (SV.slice py) v)

slice :: forall (x :: Nat) (y :: Nat) r' c' r c m n a.
         ( KnownNat r'
         , KnownNat c'
         , KnownNat x
         , KnownNat y
         , r ~ ((x + r') + m)
         , c ~ ((y + c') + n)
         )
      => (Proxy x, Proxy y)
      -> Lens' (Rect r c a)
               (Rect r' c' a)
slice (rp, cp) = lens get' set'
  where
    get' :: Rect r c a -> Rect r' c' a
    get' = sliceC cp . sliceR rp

    set' = panic "subRect.set'"

-- subRect :: forall r c r' c' y x y' x' a.
--            ( KnownNat r
--            , KnownNat c
--            -- , KnownNat x
--            -- , KnownNat y
--            -- , KnownNat x'
--            -- , KnownNat y'
--            , y ~ Finite r
--            , x ~ Finite c
--            -- , y' ~ Finite
--            -- , x' ~ Finite c
--            -- , y <= y'
--            -- , x <= x'
--            )
--         => ( ( y
--              , x
--              )
--            , ( y'
--              , x'
--              )
--            )
--         -> Lens' (Rect r  c  a)
--                  (Rect r' c' a)
-- subRect ((_y, x), (_y', _x')) = lens get' set'
--   where
--     get' :: Rect r c a -> Rect r' c' a
--     get' (Rect vv) = Rect vv''
--       where
--         _ = vv :: SV.Vector c (SV.Vector r a)

--         vv'' :: SV.Vector c' (SV.Vector r' a)
--         vv'' = SV.map crunch vv'

--         crunch :: SV.Vector r  a
--                -> SV.Vector r' a
--         crunch = undefined -- takeFront . dropFront

--         vv' :: SV.Vector c' (SV.Vector r a)
--         vv'= takeFront . dropFront $ vv
--           where
--             snxm :: Maybe SomeNat
--             snxm = someNatVal snxSource

--             snxSource :: Integer
--             snxSource = fromIntegral x

--             snx :: SomeNat
--             snx = case snxm of
--               Nothing -> panic "subRect.snx"
--               Just  v -> v

--             dproxy :: Proxy d
--             dproxy = case snx of
--               SomeNat p -> Proxy

--             tproxy :: Proxy t
--             tproxy = undefined

--             dropFront :: SV.Vector  c      q
--                       -> SV.Vector (c - d) q
--             dropFront = undefined -- SV.drop

--             takeFront :: SV.Vector t z
--                       -> SV.Vector c' z
--             takeFront = SV.take

--     set' :: Rect r c a -> Rect r' c' a -> Rect r c a
--     set' = panic "subRect.set'"

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
      => Rect r ca a
      -> Rect r cb a
      -> Rect r cc a
a <//> b = transpose (transpose a <+> transpose b)

(<+>) :: forall ra rb rc c a.
         ( rc ~ (ra + rb) )
      => Rect ra c a
      -> Rect rb c a
      -> Rect rc c a
Rect a <+> Rect b = Rect $ SV.zipWith (SV.++) a b

transpose :: forall r c a.
             ( KnownNat r
             , KnownNat c
             )
          => Rect r c a
          -> Rect c r a
transpose = unRect >>> SV.transpose >>> Rect

zipWith :: forall r c a b z.
           ( KnownNat r
           , KnownNat c
           )
        => (a -> b -> z)
        -> Rect r c a
        -> Rect r c b
        -> Rect r c z
zipWith f a b = f <$> a <*> b

zip :: forall r c a b.
       ( KnownNat r
       , KnownNat c
       )
    => Rect r c a
    -> Rect r c b
    -> Rect r c (a, b)
zip = zipWith (,)

-- ================================================================ --
--   Eliminators
-- ================================================================ --

toLists :: forall r c a.
           Rect r c a
        -> [[a]]
toLists = unRect
      >>> map SV.toList
      >>> SV.toList

toTexts :: forall r c a.
           Renderable a
        => Rect r c a
        -> [[Text]]
toTexts = toLists
      >>> map (map render)

unRect :: Rect r c a -> SV.Vector c (SV.Vector r a)
unRect (Rect crv) = crv
