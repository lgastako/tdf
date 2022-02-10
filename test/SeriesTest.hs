{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module SeriesTest where

import           TDF.Prelude

import qualified Data.Set         as Set
import           Orphans                    ()
import           Test.Tasty.Hspec
import           Test.QuickCheck
import qualified TDF.Index        as Index
import           TDF.Series                 ( Series )
import qualified TDF.Series       as Series
import qualified Data.Vec.Lazy    as Vec

spec_Series :: Spec
spec_Series = do
  context "with simple series" $ do
    let s :: Series Nat3 Int Float
        s = case Series.fromList [1.1, 2.2, 3.3] of
              Nothing -> panic "aaaaaah"
              Just  x -> x

        ss :: Series Nat6 Int Float
        ss = Series.append s s

    it "should self append" $
      length ss `shouldBe` 6

prop_Append_NonOverlappingIndices :: Series Nat2 Int Bool
                                  -> Series Nat3 Int Bool
                                  -> Property
prop_Append_NonOverlappingIndices a b =
  Set.size idxSet
  ===
  length ab
  where
    idxSet = Set.fromList . Vec.toList $ uniqIdxes

    uniqIdxes :: Vec Nat5 Int
    uniqIdxes = Index.toVec idx

    idx = Series.index ab
    ab  = Series.append a b
