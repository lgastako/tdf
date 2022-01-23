{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TDF.Types.SAI
  ( Index
  , defaultFor
  , defaultFromFor
  , drop
  , fromList
  , index
  , toList
  , length
  , take
  ) where

import                   TDF.Prelude         hiding ( drop
                                                    , length
                                                    , take
                                                    , toList
                                                    )

import qualified Data.List            as List
import qualified Data.Vector          as Vector
import           TDF.Types.RangeIndex               ( RangeIndex )
import qualified TDF.Types.RangeIndex as RangeIndex

data Index idx
  = ListIdx  [idx]
  | RangeIdx (RangeIndex idx)
  deriving (Eq, Generic, Ord, Show)

instance NFData idx => NFData (Index idx)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromList :: [idx] -> Index idx
fromList = ListIdx

defaultFor :: Foldable f
           => f a
           -> Index Int
defaultFor = defaultFromFor 0

defaultFromFor :: Foldable f
               => Int
               -> f a
               -> Index Int
defaultFromFor n = RangeIdx . RangeIndex.defaultFromFor n

-- ================================================================ --
--   Combinators
-- ================================================================ --

drop :: ( Enum idx
        , Eq idx
        , Num idx
        )
     => Int
     -> Index idx
     -> Index idx
drop n = \case
  ListIdx  ixs -> ListIdx  $ List.drop n ixs
  RangeIdx rix -> RangeIdx $ RangeIndex.drop n rix

take :: ( Enum idx
        , Eq idx
        , Num idx
        )
     => Int
     -> Index idx
     -> Index idx
take n = \case
  ListIdx  ixs -> ListIdx  $ List.take n ixs
  RangeIdx rix -> RangeIdx $ RangeIndex.take n rix

-- ================================================================ --
--   Eliminators
-- ================================================================ --

index :: (Enum idx, Eq idx, Num idx)
      => Index idx
      -> Vector a
      -> Vector (idx, a)
index = Vector.zip . Vector.fromList . toList

length :: Enum idx => Index idx -> Int
length = \case
  ListIdx  ixs -> List.length ixs
  RangeIdx rix -> RangeIndex.length rix

toList :: ( Enum idx
           , Eq idx
           , Num idx
           )
        => Index idx
        -> [idx]
toList = \case
  ListIdx  ixs -> ixs
  RangeIdx rix -> RangeIndex.toList rix
