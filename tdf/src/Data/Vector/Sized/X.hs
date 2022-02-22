{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Vector.Sized.X
  ( module X
  , ToVectorN(..)
  ) where

import Data.Vector.Sized as X

import Data.Frame.Prelude ( KnownNat
                          , orCrash
                          )
import Data.List.NonEmpty ( NonEmpty )

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized  as Sized

class ToVectorN x n a where
  toVectorN :: x -> Sized.Vector n a

instance ToVectorN (Sized.Vector n a) n a where
  toVectorN = id

instance KnownNat n => ToVectorN a n a where
  toVectorN = Sized.replicate

instance KnownNat n => ToVectorN (NonEmpty a) n a where
  toVectorN = orCrash "toVecn" . Sized.fromList . NE.toList
