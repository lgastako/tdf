{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
  -- Optics
  , index
  , vector
  -- Combinators
  , (++)
  -- Eliminators
  , thaw
  ) where

import Data.Grid.Prelude hiding ( (++)
                                , empty
                                )

import Data.Grid.Index          ( Index )

import qualified Data.Grid.Index          as I
import qualified Data.Vector.Sized        as Sized
import qualified Data.Grid.Series.Mutable as M

newtype Series (n :: Nat) k a = Series
  ( Index n k
  , Sized.Vector n a
  ) deriving (Eq, Functor, Foldable, Generic, Ord, Show, Traversable)

instance ( Universal a
         , Universal idx
         ) => Universal (Series n idx a)

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: Enum k => Series 0 k a
empty = Series (I.empty, Sized.empty)

fromList :: forall n k a.
                ( Enum k
                , KnownNat n
                )
             => [a]
             -> Maybe (Series n k a)
fromList = fromListWith I.default_

fromListWith :: forall n k a.
                ( KnownNat n )
             => Index n k
             -> [a]
             -> Maybe (Series n k a)
fromListWith idx = map (Series . (idx,)) . Sized.fromList

fromVector :: forall n k a.
              ( Enum k
              , KnownNat n
              )
           => Sized.Vector n a
           -> Series n k a
fromVector = fromVectorWith I.default_

fromVectorWith :: forall n k a.
                  ( KnownNat n )
               => Index n k
               -> Sized.Vector n a
               -> Series n k a
fromVectorWith idx v = Series (idx, v)

single :: forall a k.
          ( Enum k )
       => a
       -> Series 1 k a
single x = Series (I.default_, Sized.singleton x)

singleWith :: forall k a.
              Index 1 k
           -> a
           -> Series 1 k a
singleWith idx x = Series (idx, Sized.singleton x)

-- ================================================================ --
--   Optics
-- ================================================================ --

index :: forall n k a.
         Lens' (Series n k a)
               (Index n k)
index = coerced . (_1 @(Index n k, Sized.Vector n a))

vector :: forall n k a.
          Lens' (Series n k a)
                (Sized.Vector n a)
vector = coerced . (_2 @(Index n k, Sized.Vector n a))

-- ================================================================ --
--   Combinators
-- ================================================================ --

(++) :: Series m k a -> Series n k a -> Series (m + n) k a
a ++ b = Series (ix', v')
  where
    ix' = (a ^. index)  I.++     (b ^. index)
    v'  = (a ^. vector) Sized.++ (b ^. vector)

-- ================================================================ --
--   Eliminators
-- ================================================================ --

thaw :: forall n k mm s a.
        ( Applicative mm
        , KnownNat n
        , PrimMonad mm
        , s ~ PrimState mm
        )
     => Series n k a
     -> mm (M.MSeries n k s a)
thaw s = M.fromVectorWith (s ^. index) =<< Sized.thaw (s ^. vector)

-- Temp example
_s :: Series 3 Int Float
_s = fromList [1..3] `onCrash` "Gird.series.s.boom"

