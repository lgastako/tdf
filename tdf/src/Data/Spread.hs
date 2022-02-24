{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Spread
  ( Spread(..)
  , empty
  , emptyCols
  , emptyRows
  ) where

import Data.Grid.Prelude hiding ( empty )

import Data.Holmes
import Data.Square ( Square )

import qualified Data.Square as F

newtype Spread (r :: Nat) (c :: Nat) a = Spread
  { unSpread :: Square r c a }
  deriving (Eq, Ord, Show)

empty :: forall a. Spread 0 0 a
empty = Spread F.empty

emptyCols :: forall n a. KnownNat n => Spread 0 n a
emptyCols = Spread F.emptyCols

emptyRows :: forall n a. Spread n 0 a
emptyRows = Spread F.emptyRows

constraints :: MonadCell m
            => [Prop m (Defined Bool)]
            -> Prop m (Defined Bool)
constraints = undefined
