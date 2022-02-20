{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Grid.Series.Mutable
  ( MSeries
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

import qualified Data.Grid.Index             as Index
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable.Sized.X as Sized

newtype MSeries (n :: Nat) idx s a = MSeries
  ( Index (n :: Nat) idx
  , Sized.MVector (n :: Nat) s a
  )

empty ::forall k mm s a.
        ( Enum k
        , PrimMonad mm
        , s ~  PrimState mm
        )
      => mm (MSeries 0 k s a)
empty = (MSeries . (Index.empty,)) <$> Sized.new

fromList :: forall n k mm a.
                ( Enum k
                , KnownNat n
                , PrimMonad mm
                )
             => [a]
             -> mm (Maybe (MSeries n k (PrimState mm) a))
fromList = fromListWith Index.default_

fromListWith :: forall n k mm s a.
                ( KnownNat n
                , PrimMonad mm
                , s ~ PrimState mm
                )
             => Index n k
             -> [a]
             -> mm (Maybe (MSeries n k s a))
fromListWith idx xs = V.thaw (V.fromList xs)
  <&> (fmap (MSeries . (idx,)) . Sized.toSized @n)

fromVector :: forall n k mm s a.
              ( Applicative mm
              , Enum k
              , KnownNat n
              )
           => Sized.MVector (n :: Nat) s a
           -> mm (MSeries n k s a)
fromVector = pure . MSeries . (Index.default_,)

fromVectorWith :: forall n k mm s a.
                  ( Applicative mm
                  , KnownNat n
                  )
               => Index n k
               -> Sized.MVector (n :: Nat) s a
               -> mm (MSeries n k s a)
fromVectorWith idx = pure . MSeries . (idx,)

single :: forall k mm a.
          ( Enum k
          , Monad mm
          , PrimMonad mm
          )
       => a
       -> mm (MSeries 1 k (PrimState mm) a)
single x = singleWith Index.default_ x

singleWith :: forall k mm a.
              ( Monad mm
              , PrimMonad mm
              )
           => Index 1 k
           -> a
           -> mm (MSeries 1 k (PrimState mm) a)
singleWith idx x = do
  v <- Sized.single x
  fromVectorWith idx v
