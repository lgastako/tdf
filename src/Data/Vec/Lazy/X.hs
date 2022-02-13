{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}

module Data.Vec.Lazy.X
  ( module Data.Vec.Lazy
  , AVec
  , avec
  , find
  , filter
  , recoverVec
  ) where

import           Prelude             hiding ( filter
                                            , foldr
                                            )

import qualified Data.List          as List
import           Data.Type.Equality         ( (:~:)(Refl)
                                            , testEquality
                                            )
import           Data.Type.Nat              ( Nat
                                            , Nat
                                            , SNat( -- SS
                                                  -- , SZ
                                                  )
                                            , SNatI
--                                             , fromNatural
                                            , snat
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
avec x = AVec n x
  where
    n = snat @n

recoverVec :: forall n a. SNatI n => AVec a -> Maybe (Vec n a)
recoverVec (AVec n xs) = case testEquality n n' of
  Just Refl -> Just xs
  Nothing   -> Nothing
  where
    n' = snat @n

find :: (a -> Bool) -> Vec n a -> Maybe a
find p = foldr f Nothing
  where
    f _ found@(Just _) = found
    f x Nothing | p x = Just x
                | otherwise = Nothing

filter :: forall n a.
          ( SNatI n )
       => (a -> Bool)
       -> Vec n a
       -> (SNat n, AVec a)
filter p v = package $ foldr f [] v
  where
    f :: a -> [a] -> [a]
    f x acc | p x = (x:acc)
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
          0 -> error "filter.1" --SZ
          _n -> error "filter.2" -- intToSnat . pred $ n

        _n :: Nat
        _n = fromIntegral len

        len :: Int
        len = List.length xs
