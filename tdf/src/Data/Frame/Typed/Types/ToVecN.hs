{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Data.Frame.Typed.Types.ToVecN
  ( ToVecN(..)
  ) where

import           Data.Frame.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Vec.Lazy.X    as Vec

class ToVecN x n a where
  toVecN :: x -> Vec n a

instance ToVecN (Vec n a) n a where
  toVecN = identity

instance SNatI n => ToVecN a n a where
  toVecN = Vec.repeat

instance SNatI n => ToVecN (NonEmpty a) n a where
  toVecN = Vec.dwimFromList . NE.toList
