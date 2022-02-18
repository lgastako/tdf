{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TDF.DeleteMe where

import TDF.Prelude

import           TDF.Series     ( Series )
import qualified TDF.Series as S

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
    mk xs = S.fromList xs `orAbortWith` "pairOfSeries"
