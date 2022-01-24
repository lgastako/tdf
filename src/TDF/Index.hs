{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.Index
  ( Index
  -- Constructors
  , defaultFromFor
  , defaultIntsFor
  , fromList
  , fromVec
  -- Combinators
  , drop
  , take
  -- Eliminators
  , index
  , length
  , toVec
  ) where

import           TDF.Prelude      hiding ( drop
                                         , length
                                         , take
                                         , toList
                                         )

import qualified Data.Foldable    as F
import qualified Data.Vec.Lazy    as Vec

newtype Index n idx = Index { toVec :: Vec n idx }
  deriving (Foldable, Functor, Eq, Generic, Ord, Show)

instance NFData idx => NFData (Index n idx)

-- ================================================================ --
-- Constructors
-- ================================================================ --

defaultFromFor :: forall idx f n a.
                  ( Enum idx
                  , Foldable f
                  , SNatI n
                  )
               => idx
               -> f a
               -> Maybe (Index n idx)
defaultFromFor k xs = Index <$> Vec.fromList c
  where
    as :: [a]
    as = F.toList xs

    ixs :: [idx]
    ixs = [k..]

    c :: [idx]
    c = zipWith const ixs as

defaultIntsFor :: (Foldable f, SNatI n) => f a -> Maybe (Index n Int)
defaultIntsFor = defaultFromFor 0

fromList :: SNatI n => [idx] -> Maybe (Index n idx)
fromList = fromVec <<$>> Vec.fromList

-- TODO: confirm the indexes are unique...do they need to be?
fromVec :: Vec n idx -> Index n idx
fromVec = Index

-- ================================================================ --
-- Combinators
-- ================================================================ --

drop :: forall n m idx.
        ( LE n m
        , SNatI n
        , SNatI m
        )
     => Index m idx
     -> Index n idx
drop (Index v) = Index (Vec.drop v)

take :: forall n m idx.
        LE n m
     => Index m idx
     -> Index n idx
take (Index v) = Index (Vec.take v)

-- ================================================================ --
-- Eliminators
-- ================================================================ --

index :: forall n m idx a.
         ( Enum idx
         , Eq idx
         , LE n m
         , Num idx
         , SNatI n
         , SNatI m
         )
      => Index m idx
      -> Vec n a
      -> Vec n (idx, a)
index (Index ixs) xs = (Vec.fromList . F.toList $ xs)
  & maybe error (Vec.zipWith (,) (Vec.take ixs))
  where
    error = panic "Index.index.1"

length :: Index n idx -> Int
length = Vec.length . toVec
