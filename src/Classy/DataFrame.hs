{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Classy.DataFrame where

class Frameable a where
  data Label a :: *
  data Key a   :: *

  label   :: a -> Label a
  valueAt :: Key a -> a -> b

class Frameable a => DataFrame f a where
  headN :: Int -> f a -> f a
  headN = undefined

  tailN :: Int -> f a -> f a
  tailN = undefined

--  column ::

  {-# MINIMAL headN, tailN #-}

  head :: f a -> f a
  head = headN 1

  tail :: f a -> f a
  tail = tailN 1
