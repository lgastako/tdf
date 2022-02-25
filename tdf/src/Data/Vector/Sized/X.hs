{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vector.Sized.X
  ( module X
  , (!!)
  , ToVectorN(..)
  , rangeParts
  , transpose
  , unsafeFromList
  ) where

import Prelude     hiding ( (!!)
                          , last
                          , tail
                          , zipWith
                          )

import Control.Arrow      ( (>>>) )
import Control.Lens       ( (^.)
                          , Prism'
                          , prism
                          )
import Data.Finite        ( Finite )
import Data.Frame.Prelude ( KnownNat
                          , Proxy( Proxy )
                          , orCrash
                          )
import Data.List.NonEmpty ( NonEmpty )
import Data.Maybe         ( fromMaybe )

import Data.Vector.Sized            as X

import qualified Data.List          as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized  as SV

class ToVectorN x n a where
  toVectorN :: x -> SV.Vector n a

instance ToVectorN (SV.Vector n a) n a where
  toVectorN = id

instance KnownNat n => ToVectorN a n a where
  toVectorN = SV.replicate

instance KnownNat n => ToVectorN (NonEmpty a) n a where
  toVectorN = orCrash "toVecn" . SV.fromList . NE.toList

(!!) :: SV.Vector n a
     -> Finite n
     -> a
(!!) = SV.index

rangeParts :: forall n k a b.
              ( Enum k
              , KnownNat n
              , a ~ Vector n k
              , b ~ (Int, (k, k))
              )
           => Prism' a b
rangeParts = prism g s
  where
    g (step, (start, stop)) = fromMaybe boom . SV.fromList $
      [ start
      , toEnum (fromEnum start + step)
        .. stop
      ]

    boom :: a
    boom = error "rangeParts: this should not have happened"

    s v = case rangePartsFrom v of
      Nothing -> Left v
      Just parts -> Right parts

rangePartsFrom :: forall n a.
                  ( Enum a
                  , KnownNat n
                  )
               => Vector n a
               -> Maybe (Int, (a, a))
rangePartsFrom v
  | nDistinctSteps == 1 = Just (step', (start', stop'))
  | otherwise           = Nothing
  where
    l :: [a]
    l = toList v

    t :: [a]
    t = L.tail l

    steps:: [Int]
    steps = L.zipWith f t l

    step'  = fromEnum next' - fromEnum start'
    start' = v ^. ix 0
    next'  = v ^. ix 1
    stop'  = L.last l

    f :: a -> a -> Int
    f a b = fromEnum a - fromEnum b

    nDistinctSteps :: Int
    nDistinctSteps = Prelude.length . L.nub . L.sort $ steps

transpose :: forall n m a.
             ( KnownNat n
             , KnownNat m
             )
          => Vector n (Vector m a)
          -> Vector m (Vector n a)
transpose = SV.toList
  >>> fmap SV.toList
  >>> L.transpose
  >>> fmap unsafeFromList
  >>> unsafeFromList

unsafeFromList :: forall n a.
                  KnownNat n
               => [a]
               -> Vector n a
unsafeFromList xs = fromMaybe boom . SV.fromList $ xs
  where
    p :: Proxy n
    p = Proxy

    boom = error $ "Vector.Sized.X.unsafeFromList: expected "
        <> show p -- TODO
        <> ", got="
        <> show (Prelude.length xs)
