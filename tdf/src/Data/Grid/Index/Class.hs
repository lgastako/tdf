{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Data.Grid.Index.Class
  ( AnyIndex(..)
  ) where

import Data.Grid.Prelude

import qualified Data.Vector.Sized as Sized

class AnyIndex a (n :: Nat) ix where
  toVector :: a -> Sized.Vector n ix
