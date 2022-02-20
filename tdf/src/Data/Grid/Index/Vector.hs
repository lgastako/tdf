{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Grid.Index.Vector
  ( VectorIndex
  -- Constructors
  , fromVector
  -- Combinators
  , (++)
  , inc
  , incBy
  ) where

import Data.Grid.Prelude hiding ( (++) )

import Data.Grid.Index.Class ( AnyIndex( toVector ) )

import qualified Data.Vector.Sized as Sized

newtype VectorIndex (n :: Nat) k = VectorIndex
  { unVectorIndex :: Sized.Vector n k }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance AnyIndex (VectorIndex n k) n k where
  toVector = unVectorIndex

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromVector :: Sized.Vector n k -> VectorIndex n k
fromVector = VectorIndex

-- ================================================================ --
--   Optics
-- ================================================================ --

_rep :: Iso' (VectorIndex n k) (Sized.Vector n k)
_rep = iso unVectorIndex VectorIndex

-- ================================================================ --
--   Combinators
-- ================================================================ --

(++) :: forall m n k.
        ( Enum k
        , Ord k
        )
     => VectorIndex m k
     -> VectorIndex n k
     -> VectorIndex (m + n) k
_a ++ _b = panic "++ undefined"

inc :: forall n k.
         ( Enum k
         , KnownNat n
         )
      => VectorIndex n k
      -> VectorIndex n k
inc = incBy 1

incBy :: forall n k.
         ( Enum k
         , KnownNat n
         )
      => Int
      -> VectorIndex n k
      -> VectorIndex n k
incBy n = fmap (addToEnum n)
