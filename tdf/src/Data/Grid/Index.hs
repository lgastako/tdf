{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Grid.Index
  ( Index
  -- , MultiIndex
  , RangeIndex
  -- Constructors
  , default_
  , empty
  -- Combinators
  , append
  ) where

import Data.Grid.Prelude hiding ( empty )

import Data.Grid.Index.Range    ( RangeIndex )
import Data.Grid.Index.Vector   ( VectorIndex )

import qualified Data.Grid.Index.Range as RangeIndex

data Index (n :: Nat) k
  = IdxRange    (RangeIndex    n k)
  | IdxVector   (VectorIndex   n k)
deriving instance Eq   k => Eq   (Index n k)
deriving instance Ord  k => Ord  (Index n k)
deriving instance Show k => Show (Index n k)

instance Universal k => Universal (Index n k)

-- ================================================================ --
--   Constructors
-- ================================================================ --

default_ :: forall n k.
            ( Enum k
            , KnownNat n
            )
         => Index n k
default_ = panic "Index.default_"

empty :: Enum k => Index 0 k
empty = IdxRange RangeIndex.empty

-- ================================================================ --
--   Combinators
-- ================================================================ --

append :: forall m n k. Index m k -> Index n k -> Index (m + n) k
append = panic "Grid.Index.append"
