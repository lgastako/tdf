{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vector.Mutable.Sized.X
  ( module X
  , single
  ) where

import Control.Monad.Primitive ( PrimMonad
                               , PrimState
                               )

import Data.Vector.Mutable.Sized as X

single :: forall m a. PrimMonad m
       => a
       -> m (MVector 1 (PrimState m) a)
single = replicate
