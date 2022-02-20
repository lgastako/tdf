{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Data.Grid.Series
  ( Series
  -- Constructors
  , empty
  , fromList
  , fromListWith
  , fromVector
  , fromVectorWith
  , single
  , singleWith
  ) where

import Data.Grid.Prelude hiding ( empty )

import Data.Grid.Index          ( Index )

import qualified Data.Grid.Index   as Index
import qualified Data.Vector.Sized as Sized

newtype Series (n :: Nat) k a = Series
  ( Index (n :: Nat) k
  , Sized.Vector n a
  ) deriving (Eq, Functor, Foldable, Ord, Show, Traversable)

instance ( Universal a
         , Universal idx
         ) => Universal (Series n idx a)

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: Enum k => Series 0 k a
empty = Series (Index.empty, Sized.empty)

fromList :: forall n k a.
                ( Enum k
                , KnownNat n
                )
             => [a]
             -> Maybe (Series n k a)
fromList = fromListWith Index.default_

fromListWith :: forall n k a.
                ( KnownNat n )
             => Index n k
             -> [a]
             -> Maybe (Series n k a)
fromListWith ix = map (Series . (ix,)) . Sized.fromList

fromVector :: forall n k a.
              ( Enum k
              , KnownNat n
              )
           => Sized.Vector n a
           -> Series n k a
fromVector = fromVectorWith Index.default_

fromVectorWith :: forall n k a.
                  ( KnownNat n )
               => Index n k
               -> Sized.Vector n a
               -> Series n k a
fromVectorWith ix v = Series (ix, v)

single :: forall a k.
          ( Enum k )
       => a
       -> Series 1 k a
single x = Series (Index.default_, Sized.singleton x)

singleWith :: forall k a.
              Index 1 k
           -> a
           -> Series 1 k a
singleWith ix x = Series (ix, Sized.singleton x)
