{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.Index
  ( Index
  -- Constructors
  , defaultFromFor
  , defaultIntsFor
  , fromList
  , fromVec
  -- Combinators
  , append
  , drop
  , tail
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
  deriving (Foldable, Functor, Eq, Generic, Ord, Show, Traversable)

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

append :: forall n m idx.
          ( Num idx
          , Ord idx
          )
       => Index n idx
       -> Index m idx
       -> Index (Plus n m) idx
append (Index a) (Index b) = Index
  . (Vec.++) a
  . Vec.map (+offset)
  $ b
  where
    offset :: idx
    offset | delta >= 1 = 0
           | otherwise = maxLeft - minRight + 1

    delta    = minRight - maxLeft
    maxLeft  = maximum a
    minRight = minimum b

--  (0, 2)       (3, 5)
--  [0, 1, 2]    [3, 4, 5]
--
--    maxLeft=2
--    minRight=3

--  (0, 2)       (0, 2)
--  [0, 1, 2]    [0, 1, 2]
--
--    maxLeft=2
--    minRight=0

--  (0, 2)       (1, 3)
--  [0, 1, 2]    [1, 2, 3]
--
--    maxLeft=2
--    minRight=1
--    delta = 2



drop :: forall n m idx.
        ( LE n m
        , SNatI n
        , SNatI m
        )
     => Index m idx
     -> Index n idx
drop (Index v) = Index (Vec.drop v)

tail :: forall n m idx.
        LE n m
     => Index m idx
     -> Index n idx
tail (Index v) = Index (Vec.take v)  -- TODO

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
