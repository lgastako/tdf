{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Types.RangeIndex
  ( RangeIndex( RangeIndex )
  , size
  , stepping
  , through
  , toList
  , upTo
  ) where

import GHC.Generics ( Generic )
import Data.Text    ( Text )

data RangeIndex a = RangeIndex
  { start :: a
  , stop  :: a
  , step  :: a
  , name  :: Maybe Text
  } deriving (Eq, Generic, Ord, Read, Show)

size :: Num a => RangeIndex a -> a
size RangeIndex {..} = stop - start

stepping :: a -> a -> a -> RangeIndex a
stepping a b c = RangeIndex a b c Nothing

through :: Num a => a -> a -> RangeIndex a
through a b = RangeIndex
  { start = a
  , stop  = b
  , step  = 1
  , name  = Nothing
  }

toList :: (Enum a, Eq a, Num a) => RangeIndex a -> [a]
toList RangeIndex {..} = [start, start + step .. stop + offset]
  where
    offset | signum step == -1 = 1
           | otherwise         = -1

upTo :: Num a => a -> RangeIndex a
upTo = (0 `through`)
