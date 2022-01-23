{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TDF.Types.SI
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

-- import qualified Data.List            as List
import qualified Data.Vector          as Vector
-- import           TDF.Types.RangeIndex               ( RangeIndex )
-- import qualified TDF.Types.RangeIndex as RangeIndex

data Index idx = Index
  deriving (Eq, Generic, Ord, Show)

instance NFData idx => NFData (Index idx)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromList :: [idx] -> Index idx
fromList = panic "SII.fromList"

defaultFor :: Foldable f
           => f a
           -> Index Int
defaultFor = defaultFromFor 0

defaultFromFor :: Foldable f
               => Int
               -> f a
               -> Index Int
defaultFromFor _n = panic "SII.defaultFromFor"

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
drop _n = panic "SII.drop n"

take :: ( Enum idx
        , Eq idx
        , Num idx
        )
     => Int
     -> Index idx
     -> Index idx
take _n = panic "SII.take n"

-- ================================================================ --
--   Eliminators
-- ================================================================ --

index :: (Enum idx, Eq idx, Num idx)
      => Index idx
      -> Vector a
      -> Vector (idx, a)
index = Vector.zip . Vector.fromList . toList

length :: Enum idx => Index idx -> Int
length = panic "SII.length"

toList :: ( Enum idx
           , Eq idx
           , Num idx
           )
        => Index idx
        -> [idx]
toList = panic "SII.length"
