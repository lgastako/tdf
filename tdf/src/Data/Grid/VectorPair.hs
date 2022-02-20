{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Grid.VectorPair
  ( VectorPair
  -- Constructors
  , fromVectors
  ) where

import Data.Grid.Prelude

import Data.Grid.Index.Class ( AnyIndex( toVector ) )

import qualified Data.Vector.Sized as Sized

data VectorPair (n :: Nat) k = forall a b. (n ~ (a + b))
  => VectorPair ( Sized.Vector a k
                , Sized.Vector b k
                )

instance AnyIndex (VectorPair n k) n k where
  toVector (VectorPair (a, b)) = a Sized.++ b

fromVectors :: forall n a b x.
               ( n ~ (a + b) )
            => Sized.Vector a x
            -> Sized.Vector b x
            -> VectorPair n x
fromVectors = VectorPair ... (,)
