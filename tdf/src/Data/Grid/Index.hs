{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Grid.Index
  ( Index
  -- , MultiIndex
  , RangeIndex
  -- Constructors
  , default_
  , empty
  -- Combinators
  , (++)
  , append
  ) where

import Data.Grid.Prelude   hiding ( (++)
                                  , empty
                                  )

import Data.Grid.Index.Class      ( AnyIndex( toVector ) )
import Data.Grid.Index.Range      ( RangeIndex )
import Data.Grid.Index.Vector     ( VectorIndex )

import qualified Data.Grid.Index.Class  as I
import qualified Data.Grid.Index.Range  as RI
import qualified Data.Grid.Index.Vector as VI
--import qualified Data.Vector.Sized      as Sized

data Index (n :: Nat) k
  = IdxRange    (RangeIndex    n k)
  | IdxVector   (VectorIndex   n k)
deriving instance Eq   k => Eq   (Index n k)
deriving instance Ord  k => Ord  (Index n k)
deriving instance Show k => Show (Index n k)

instance Universal k => Universal (Index n k)

instance (Enum k, KnownNat n) => AnyIndex (Index n k) n k where
  toVector :: Index n k -> Vector n k
  toVector = \case
    IdxRange  idx -> toVector idx
    IdxVector idx -> toVector idx

-- ================================================================ --
--   Constructors
-- ================================================================ --

default_ :: forall n k.
            ( Enum k
            , KnownNat n
            )
         => Index n k
default_ = IdxRange $ RI.fromTo (toEnum 0) (toEnum n)
  where
    n = fromIntegral (natVal @n Proxy)

empty :: Enum k => Index 0 k
empty = IdxRange RI.empty

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
        IdxVector $ (VI.fromVector . I.toVector $ ria) VI.++ vib
      (IdxVector via , IdxRange rib) ->
        IdxVector $ via VI.++ (VI.fromVector . I.toVector $ rib)

-- TODO do we really need uniqueness checking?
append :: forall m n k. Index m k -> Index n k -> Index (m + n) k
append = panic "Grid.Index.append"
