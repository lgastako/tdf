{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Grid.Index
  ( Index
  -- , MultiIndex
  , RangeIndex
  -- Constructors
  , empty
  , fill
  , single
  -- Combinators
  , (++)
  , apForSeries
  , append
  , position
  , zipWith
  , zip
  -- Eliminators
  , toVector
  ) where

import Data.Grid.Prelude   hiding ( (++)
                                  , empty
                                  , zip
                                  , zipWith
                                  )

import Data.Grid.Index.Range      ( RangeIndex )
import Data.Grid.Index.Vector     ( VectorIndex )

import qualified Data.Grid.Index.Range  as RI
import qualified Data.Grid.Index.Vector as VI
import qualified Data.Vector.Sized      as SV

data Index (n :: Nat) k
  = IdxRange    (RangeIndex    n k)
  | IdxVector   (VectorIndex   n k)
deriving instance Eq   k => Eq   (Index n k)
deriving instance Ord  k => Ord  (Index n k)
deriving instance Show k => Show (Index n k)

instance Universal k => Universal (Index n k)

instance (Enum k, KnownNat n) => ToVectorN (Index n k) n k where
  toVectorN = \case
    IdxRange  idx -> RI.toVector idx
    IdxVector idx -> VI.toVector idx

-- ================================================================ --
--   Constructors
-- ================================================================ --

fill :: forall n k.
            ( Enum k
            , KnownNat n
            )
         => Index n k
fill = IdxRange $ RI.fromTo (toEnum 0) (toEnum n)
  where
    n = fromIntegral (natVal @n Proxy)

empty :: Enum k => Index 0 k
empty = IdxRange RI.empty

single ::  k -> Index 1 k
single = IdxVector . VI.fromVector . SV.singleton

-- ================================================================ --
--   Combinators
-- ================================================================ --

(++) :: forall m n k.
        ( Enum k
        , KnownNat m
        , KnownNat n
        , Ord k
        )
     => Index m k
     -> Index n k
     -> Index (m + n) k
(++) = curry f
  where
    f :: (Index m k,  Index n k) -> Index (m + n) k
    f = \case
      (IdxRange ria , IdxRange  rib) ->
        IdxRange (ria RI.++  rib)
      (IdxVector via, IdxVector vib) ->
        IdxVector (via VI.++ vib)
      (IdxRange ria , IdxVector vib) ->
        IdxVector $ (VI.fromVector . RI.toVector $ ria) VI.++ vib
      (IdxVector via , IdxRange rib) ->
        IdxVector $ via VI.++ (VI.fromVector . RI.toVector $ rib)

apForSeries :: forall n k.
               ( Enum k
               , KnownNat n
               )
            => Index n k
            -> Index n k
            -> Index n k
apForSeries = zipWith const

-- TODO do we really need uniqueness checking?
append :: forall m n k. Index m k -> Index n k -> Index (m + n) k
append = panic "Grid.Index.append"

zipWith :: forall n a b c.
           ( Enum a
           , Enum b
           , KnownNat n
           )
        => (a -> b -> c)
        -> Index n a
        -> Index n b
        -> Index n c
zipWith f ia ib = IdxVector $ VI.fromVector v'
  where
    v' = SV.zipWith f (toVector ia) (toVector ib)

zip :: forall n k k'.
       ( Enum k
       , Enum k'
       , KnownNat n
       )
    => Index n k
    -> Index n k'
    -> Index n (k, k')
zip = zipWith (,)

-- ================================================================  --
--   Eliminators
-- ================================================================  --

position :: forall n k.
            k
         -> Index n k
         -> Maybe (Finite n)
position = panic "Grid.Index.position"

toVector :: forall n k.
            ( Enum k
            , KnownNat n
            )
         => Index n k
         -> SV.Vector n k
toVector = \case
  IdxRange  idx -> RI.toVector idx
  IdxVector idx -> VI.toVector idx
