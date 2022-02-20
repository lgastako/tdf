{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Data.Grid.Index.Class
  ( Index(..)
  ) where

import qualified Data.Vector.Sized as Sized

class Index a ix where
  toVector :: a -> Sized.Vector n ix
