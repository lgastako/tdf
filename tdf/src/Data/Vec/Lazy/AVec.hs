{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Vec.Lazy.AVec
  ( AVec(..)
  , avec
  , filter
  , recoverVec
  , reify
  ) where

import Data.Frame.Prelude hiding ( filter
                                 , foldr
                                 , zip
                                 , zipWith
                                 )


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

-- ================================================================ --
--   Constructors
-- ================================================================ --

avec :: forall n a. SNatI n => Vec n a -> AVec a
avec = AVec (snat @n)

-- ================================================================ --
--   Combinators
-- ================================================================ --

filter :: forall n a. (a -> Bool) -> Vec n a -> AVec a
filter _ VNil = AVec SZ VNil
filter p (x ::: xs)
  | p x, AVec _ v <- filter p xs = AVec SS (x ::: v)
  | otherwise = filter p xs

-- ================================================================ --
--   Eliminators
-- ================================================================ --

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
