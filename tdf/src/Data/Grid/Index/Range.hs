{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Grid.Index.Range
  ( RangeIndex
  -- Constructors
  , empty
  , fromTo
  , fromToBy
  , fromVector
  -- Optics
  , vector
  -- Combinators
  , (++)
  , inc
  , incBy
  -- Eliminators
  , start
  , step
  , stop
  , toVector
  ) where

import Data.Grid.Prelude hiding ( (++)
                                , empty
                                )

import qualified Data.Vector.Sized as Sized

newtype RangeIndex (n :: Nat) k = RangeIndex { unRangeIndex :: (Int, (k, k)) }
  deriving (Eq, Generic, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: forall k.
         Enum k
      => RangeIndex 0 k
empty = fromTo (toEnum 0) (toEnum 0)

fromTo :: forall n k.
          k
       -> k
       -> RangeIndex n k
fromTo = fromToBy 1

fromToBy :: forall n k.
            Int
         -> k
         -> k
         -> RangeIndex n k
fromToBy n a b = RangeIndex (n, (a,  b))

fromVector :: forall n m k.
              ( Enum k
              , KnownNat m
              , (((n + 1) + 1) + 1) ~ m
              )
           => Sized.Vector m k
           -> RangeIndex n k
fromVector v = RangeIndex
  ( step'
  , ( start'
    , stop'
    )
  )
  where
    step'  = toEnum $ fromEnum next' - fromEnum start'
    start' = v ^. Sized.ix 0
    next'  = v ^. Sized.ix 1
    stop'  = Sized.last v

-- ================================================================ --
--   Optics
-- ================================================================ --

rep :: Iso' (RangeIndex n k) (Int, (k, k))
rep = iso unRangeIndex RangeIndex

-- ================================================================ --
--   Combinators
-- ================================================================ --

(++) :: forall m n k.
        ( Enum k
        , Ord k
        )
     => RangeIndex m k
     -> RangeIndex n k
     -> RangeIndex (m + n) k
RangeIndex (stepA, (startA, stopA)) ++ RangeIndex (_stepB, (startB, stopB)) =
  RangeIndex (stepA, (startC, stopC))
  where
    startC :: k
    startC = startA

    stopC :: k
    stopC | startB > stopA = stopB
          | otherwise = toEnum $
            fromEnum stopB + min 0 (fromEnum startB - fromEnum stopA)

inc :: forall n k.
         ( Enum k
         , KnownNat n
         )
      => RangeIndex n k
      -> RangeIndex n k
inc = incBy 1

incBy :: forall n k.
         ( Enum k
         , KnownNat n
         )
      => Int
      -> RangeIndex n k
      -> RangeIndex n k
incBy n = rep . _2 . each %~ addToEnum n

vector :: forall n k. KnownNat n
       => Iso' (RangeIndex n k)
               (Sized.Vector n k)
-- vector = iso toVector fromVector
vector = panic "Range.vector"

-- fromVector :: forall n m k.
--               ( (((n + 1) + 1) + 1) ~ m )
--            => Sized.Vector (S (S (S n))) k
--            -> RangeIndex n k

-- toVector :: forall n k.
--             ( Enum k
--             , KnownNat n
--             )
--          => RangeIndex n k
--          -> Sized.Vector n k

-- ================================================================ --
--   Eliminators
-- ================================================================ --

next :: Enum k => Int -> k -> k
next n = ala Endo foldMap (replicate n succ)

start :: RangeIndex n k -> k
start = view (rep . _2 . _1)

step :: RangeIndex n k -> Int
step = view (rep . _1)

stop :: RangeIndex n k -> k
stop = view (rep . _2 . _2)

toVector :: forall n k.
            ( Enum k
            , KnownNat n
            )
         => RangeIndex n k
         -> Sized.Vector n k
toVector idx = Sized.unfoldrN ((,) <*> f) (start idx)
  where
    f = next (step idx)
