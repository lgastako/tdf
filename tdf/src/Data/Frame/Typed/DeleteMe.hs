{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Frame.Typed.DeleteMe where

import Data.Frame.Prelude

import           Data.Frame.Typed.Series      ( Series )
import qualified Data.Frame.Typed.Series as S

data Pair a = Pair a a
  deriving (Eq, Ord, Foldable, Functor, Show, Traversable)

toTup :: Pair a -> (a, a)
toTup (Pair a b) = (a, b)

pairOfLists :: Pair [Int]
pairOfLists = Pair [1,2,3] [4,5,6]

listOfPairs :: [Pair Int]
listOfPairs = sequenceA pairOfLists

pairOfZipLists :: Pair (ZipList Int)
pairOfZipLists = fmap ZipList pairOfLists

zipListOfPairs :: ZipList (Pair Int)
zipListOfPairs = sequenceA pairOfZipLists

pairOfSeries :: Pair (Series Nat3 Int Int)
pairOfSeries = Pair (mk [1,2,3]) (mk [4,5,6])
  where
    mk xs = S.fromList xs `onCrash` "pairOfSeries"

seriesOfPairs :: Series Nat3 Int (Pair Int)
seriesOfPairs = sequenceA pairOfSeries

wowza :: IO ()
wowza = S.display $ toTup <$> seriesOfPairs
