{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module Data.Vec.Lazy.X
  ( module Data.Vec.Lazy
  , AVec
  , avec
  , dwimFromList
  , filter
  , recoverVec
  , unsafeFromList
  ) where

import           Prelude             hiding ( filter
                                            , foldr
                                            )

import qualified Data.List          as List
import           Data.Maybe                 ( fromMaybe )
import           Data.Type.Equality         ( (:~:)(Refl)
                                            , testEquality
                                            )
import           Data.Type.Nat              ( Nat
                                            , Nat
                                            , SNat
                                            , SNatI
                                            , snat
                                            , snatToNat
                                            )
import           Data.Vec.Lazy

data AVec a = forall n. AVec (SNat n) (Vec n a)

deriving instance Show a => Show (AVec a)

instance Eq a => Eq (AVec a) where
  (==) :: AVec a -> AVec a -> Bool
  AVec n xs == AVec n' xs' = case testEquality n n' of
    Nothing   -> False
    Just Refl -> xs == xs'

avec :: forall n a. SNatI n => Vec n a -> AVec a
avec = AVec (snat @n)

recoverVec :: forall n a. SNatI n => AVec a -> Maybe (Vec n a)
recoverVec (AVec n xs) = case testEquality n n' of
  Just Refl -> Just xs
  Nothing   -> Nothing
  where
    n' = snat @n

reifyAVec :: forall a r. (forall n. SNatI n => Vec n a -> r) -> AVec a -> r
reifyAVec f (AVec n v) = g n v
  where
    _ = f :: forall m b q. SNatI m => Vec m b -> q

    g :: SNatI n => SNat n -> Vec n a -> r
    g _n = f

--     _ = f :: forall n0. SNatI n0 => Vec n0 a -> r
-- --    _ = n :: SNatI n => SNat n

--     g :: forall n a r. SNatI n => Vec n a -> r
--     g = undefined
--       where
--         m = fromIntegral . snatToNat $ snat @n

--    sn = snat @

filter :: forall n a.
          ( SNatI n )
       => (a -> Bool)
       -> Vec n a
       -> (SNat n, AVec a)
filter p v = package $ foldr f [] v
  where
    f :: a -> [a] -> [a]
    f x acc | p x       = x:acc
            | otherwise = acc

    package :: [a] -> (SNat n, AVec a)
    package xs = case fromList xs of
      Nothing -> error "filter explode"
      Just v' -> (sn, AVec sn v')
      where
        sn :: SNat n
        sn = intToSnat len

        intToSnat :: Int -> SNat n
        intToSnat = \case
          0  -> error "filter.1" -- SZ
          _n -> error "filter.2" -- intToSnat . pred $ n

        _n :: Nat
        _n = fromIntegral len

        len :: Int
        len = List.length xs

dwimFromList :: forall n a. SNatI n => [a] -> Vec n a
dwimFromList = unsafeFromList . List.take m . List.cycle
  where
    m = fromIntegral . snatToNat $ snat @n

unsafeFromList :: forall n a. SNatI n => [a] -> Vec n a
unsafeFromList = fromMaybe (error msg) . fromList
  where
    msg = "unsafeFromList called on list of wrong size."

