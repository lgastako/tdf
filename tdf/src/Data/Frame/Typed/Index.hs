{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns             #-}

module Data.Frame.Typed.Index
  ( Index
  , MultiIndex(..)
  -- Constructors
  , defaultFromFor
  , defaultIntsFor
  , fromLst
  , fromVec
  -- Combinators
  , concat
  , drop
  , take
  -- Eliminators
  , index
  , length
  , toFin
  , toFinE
  ) where

import Data.Frame.Prelude          hiding ( concat
                                          , drop
                                          , length
                                          , take
                                          , toList
                                          )

import GHC.Show                           ( Show(..) )
import Data.Frame.Typed.Index.Categorical ( CategoricalIndex )
import Data.Frame.Typed.Index.DateTime    ( DateTimeIndex )
import Data.Frame.Typed.Index.Interval    ( IntervalIndex )
import Data.Frame.Typed.Index.Period      ( PeriodIndex(..) )
import Data.Frame.Typed.Index.Range       ( RangeIndex(..) )
import Data.Frame.Typed.Index.TimeDelta   ( TimeDeltaIndex(..) )
import Data.Frame.Typed.SubIndex          ( SubIndex(..) )
import Data.Frame.Typed.Types.ToVecN      ( ToVecN( toVecN ) )

import qualified Data.Fin                           as Fin
import qualified Data.Foldable                      as F
import qualified Data.Frame.Typed.Index.Categorical as CategoricalIndex
import qualified Data.Frame.Typed.Index.Range       as RangeIndex
import qualified Data.Frame.Typed.SubIndex          as SubIndex
import qualified Data.List                          as List
import qualified Data.Vec.Lazy.X                    as Vec

-- TODO do we need ToVecN as a separate class? Can we collapse it into
-- SubIndex? Or, if not, then make it a superclass?

type MultiIndex :: Nat -> Type -> Type
data MultiIndex (n :: Nat) idx = forall l r.
  ( SNatI n
  , SNatI l
  , SNatI r
  , n ~ Plus l r
  ) => MultiIndex
    { leftIndex  :: Index l idx
    , rightIndex :: Index r idx
    }

instance Eq (MultiIndex n idx) where
  _m1 == _m2 = panic "Index.MultiIndex.Eq"

instance Ord (MultiIndex n idx) where
  compare = panic "Index.MultiIndex.Ord"

instance Show (MultiIndex n idx) where
  show = panic "Index.MultiIndex.show"

instance (Enum idx, SNatI n) => SubIndex MultiIndex n idx where
  toVec (MultiIndex a b) = SubIndex.toVec a Vec.++ SubIndex.toVec b

  drop = panic "Index.MultiIndex.drop"
  take = panic "Index.MultiIndex.take"

instance (Enum idx, SNatI n) => ToVecN (MultiIndex n idx) n idx where
  toVecN = toVec

data Index (n :: Nat) a
  = IdxCategorical (CategoricalIndex n a)
  | IdxDateTime    (DateTimeIndex n a)
  | IdxInterval    (IntervalIndex n a)
  | IdxMulti       (MultiIndex n a)
  | IdxPeriod      (PeriodIndex n a)
  | IdxRange       (RangeIndex n a)
  | IdxTimeDelta   (TimeDeltaIndex n a)
  deriving (Eq, Generic, Ord, Show)

-- We implement both toLst and toVec instead of letting one default to the
-- other at this top level, so that we delegate to the more efficient
-- specialized instances of the actual SubIndexes.
instance ( Enum a
         , SNatI n
         ) => SubIndex Index n a where

  toLst = \case
    IdxCategorical idx -> toLst idx
    IdxDateTime    idx -> toLst idx
    IdxInterval    idx -> toLst idx
    IdxMulti       idx -> toLst idx
    IdxPeriod      idx -> toLst idx
    IdxRange       idx -> toLst idx
    IdxTimeDelta   idx -> toLst idx

  toVec = \case
    IdxCategorical idx -> toVec idx
    IdxDateTime    idx -> toVec idx
    IdxInterval    idx -> toVec idx
    IdxMulti       idx -> toVec idx
    IdxPeriod      idx -> toVec idx
    IdxRange       idx -> toVec idx
    IdxTimeDelta   idx -> toVec idx

  drop = \case
    IdxCategorical idx -> IdxCategorical $ SubIndex.drop idx
    IdxDateTime    idx -> IdxDateTime    $ SubIndex.drop idx
    IdxInterval    idx -> IdxInterval    $ SubIndex.drop idx
    IdxMulti       idx -> IdxMulti       $ SubIndex.drop idx
    IdxPeriod      idx -> IdxPeriod      $ SubIndex.drop idx
    IdxRange       idx -> IdxRange       $ SubIndex.drop idx
    IdxTimeDelta   idx -> IdxTimeDelta   $ SubIndex.drop idx

  take _ = panic "Index.take"

instance (Enum idx, SNatI n) => ToVecN (Index n idx) n idx where
  toVecN = \case
    IdxCategorical idx -> toVecN idx
    IdxDateTime    idx -> toVecN idx
    IdxInterval    idx -> toVecN idx
    IdxMulti       idx -> toVecN idx
    IdxPeriod      idx -> toVecN idx
    IdxRange       idx -> toVecN idx
    IdxTimeDelta   idx -> toVecN idx

-- ================================================================ --
-- Constructors
-- ================================================================ --

defaultFromFor :: forall idx n a.
                  ( Enum idx
                  , SNatI n
                  )
               => idx
               -> Vec n a
               -> Maybe (Index n idx)
defaultFromFor idx v = Just . IdxRange $ RangeIndex.defaultFromFor idx v

defaultIntsFor :: forall n a.
                  SNatI n
               => Vec n a
               -> Maybe (Index n Int)
defaultIntsFor = defaultFromFor 0

fromLst :: forall n idx.
           SNatI n
        => [idx]
        -> Maybe (Index n idx)
fromLst = fromVec <<$>> Vec.fromList

-- TODO: confirm the indexes are unique...do they need to be?
-- TODO: This is probably completely off base
fromVec :: forall n idx. Vec n idx -> Index n idx
fromVec = IdxCategorical . CategoricalIndex.fromVec

-- ================================================================ --
-- Combinators
-- ================================================================ --

concat :: forall n m idx.
          ( Ord idx
          , SNatI n
          , SNatI m
          , SNatI (Plus n m)
          )
       => Index n idx
       -> Index m idx
       -> Index (Plus n m) idx
concat = IdxMulti ... MultiIndex

-- ================================================================ --
-- Eliminators
-- ================================================================ --

index :: forall n idx a.
         ( Enum idx
         , Eq idx
         , SNatI n
         )
      => Index n idx
      -> Vec n a
      -> Vec n (idx, a)
index = \case
  IdxCategorical idx -> SubIndex.index idx
  IdxDateTime    idx -> SubIndex.index idx
  IdxInterval    idx -> SubIndex.index idx
  IdxMulti       idx -> SubIndex.index idx
  IdxPeriod      idx -> SubIndex.index idx
  IdxRange       idx -> SubIndex.index idx
  IdxTimeDelta   idx -> SubIndex.index idx

length :: forall n idx.
          ( Enum idx
          , SNatI n
          )
       => Index n idx
       -> Int
length = Vec.length . SubIndex.toVec

toFin :: forall idx n.
         ( Enum idx
         , Eq idx
         , SNatI n
         )
      => idx
      -> Index n idx
      -> Maybe (Fin n)
toFin = either (const Nothing) Just ... toFinE

-- Atrocious, I know.  TODO.
toFinE :: forall idx n.
         ( Enum idx
         , Eq idx
         , SNatI n
         )
      => idx
      -> Index n idx
      -> Either Text (Fin n)
toFinE idx (toVec -> v) = case find ((idx ==) . snd) indexed of
  Nothing -> Left "Could not find idx in index.  Perhaps too large?"
  Just (n, _idx) -> case Fin.fromNat . fromInteger . fromIntegral $ n of
    Nothing -> Left "Could not promote index Int to Nat"
    Just result -> Right result
  where
    indexed :: [(Int, idx)]
    indexed = List.zip [0..] (F.toList v)
