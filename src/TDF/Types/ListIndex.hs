{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module TDF.Types.ListIndex
  ( ListIndex( ListIndex )
  , contains
  -- , monotonicDecreasing
  -- , monotonicIncreasing
  , size
  -- , stepping
  -- , through
  , toList
  -- , upTo
  ) where

import           TDF.Prelude              hiding ( toList )
import           TDF.Types.Index                 ( Index
                                                 , start
                                                 , stop
                                                 )
import qualified TDF.Types.Index as Index

newtype ListIndex a = ListIndex [a]
  deriving (Eq, Generic, Ord, Read, Show)

instance ( Bounded idx
         , Enum idx
         , Eq idx
         , Num idx
         )
      => Index idx (ListIndex idx) where
  start (ListIndex (x:_)) = x
  start (ListIndex [])    = panic "invalid index (case 1)"

  stop (ListIndex xs) = case lastMay xs of
    Just x  -> x
    Nothing -> panic "invalid index (case 2)"

  next (ListIndex xs) x = fromMaybe (panic error)
    . headMay
    . drop 1
    . dropWhile (/= x)
    $ xs
    where
      error = "invalid index (case 3)"

contains :: Ord a => ListIndex a -> a -> Bool
contains (ListIndex xs) n = n `elem` xs

-- monotonicDecreasing :: (Num a, Ord a) => ListIndex a -> Bool
-- monotonicDecreasing ri@(ListIndex xs) = step < 0 || size ri <= 1

-- monotonicIncreasing :: (Num a, Ord a) => ListIndex a -> Bool
-- monotonicIncreasing ri@ListIndex {..} = step > 0 || size ri <= 1

size :: (Bounded a, Enum a, Eq a, Num a) => ListIndex a -> a
size li = stop li - start li

-- stepping :: a -> a -> a -> ListIndex a
-- stepping a b c = ListIndex a b c Nothing

-- through :: Num a => a -> a -> ListIndex a
-- through a b = ListIndex
--   { start = a
--   , stop  = b
--   , step  = 1
--   , name  = Nothing
--   }

toList :: ListIndex idx -> [idx]
toList (ListIndex xs) = xs

-- upTo :: Num a => a -> ListIndex a
-- upTo = (0 `through`)

-- -- We do not implement things like `dtype` and `inferred_type` as they don't
-- -- make sense in Haskell where you always know the exact type at any given
-- -- time, and similarly we ignore things like `copy` that don't make sense for
-- -- other reasons (pervasive immutability, etc)
