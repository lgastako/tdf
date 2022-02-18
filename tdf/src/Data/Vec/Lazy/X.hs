{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Vec.Lazy.X
  ( module Data.Vec.Lazy
  , AVec(..)
  , avec
  , dwimFromList
  , filter
  , recoverVec
  , reify
  , unsafeFromList
  , zip
  ) where

import           Prelude             hiding ( filter
                                            , foldr
                                            , zip
                                            , zipWith
                                            )

import qualified Data.List          as List
import           Data.Maybe                 ( fromMaybe )
import           Data.Type.Equality         ( (:~:)(Refl)
                                            , testEquality
                                            )
import           Data.Type.Nat              ( SNat( SS
                                                  , SZ
                                                  )
                                            , SNatI
                                            , snat
                                            , snatToNat
                                            )
import           Data.Vec.Lazy

data AVec a = forall n. SNatI n => AVec
  { size :: SNat n
  , vec  :: Vec n a
  }

deriving instance Functor AVec
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

reify :: forall a r.
         (forall n. SNatI n => Vec n a -> r)
      -> AVec a
      -> r
reify f (AVec _n v) = f v

filter :: forall n a. (a -> Bool) -> Vec n a -> AVec a
filter _ VNil = AVec SZ VNil
filter p (x ::: xs)
  | p x, AVec _ v <- filter p xs = AVec SS (x ::: v)
  | otherwise = filter p xs

dwimFromList :: forall n a. SNatI n => [a] -> Vec n a
dwimFromList = unsafeFromList . List.take m . List.cycle
  where
    m = fromIntegral . snatToNat $ snat @n

unsafeFromList :: forall n a. SNatI n => [a] -> Vec n a
unsafeFromList = fromMaybe (error msg) . fromList
  where
    msg = "unsafeFromList called on list of wrong size."


zip :: Vec n a -> Vec n b -> Vec n (a, b)
zip = zipWith (,)
