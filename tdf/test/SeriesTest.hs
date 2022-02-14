{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SeriesTest where

import           TDF.Prelude

import           Orphans                    ()
import           Test.Tasty.Hspec
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

    it "should filter" $
      toList (Series.filter (<2.0) s)
        `shouldBe` [1.1]

    it "should produce these exact indexes" $ do
      let ab :: Series Nat3 Int Bool
          ab = Series.append a b

          a :: Series Nat1 Int Bool
          Just a = Series.fromList [True]

          b :: Series Nat2 Int Bool
          Just b = Series.fromList [False, False]

      (Vec.toList . Index.toVec . Series.index $ ab)
        `shouldBe` [0, 1, 2]

    it "should function applicatively" $
      (pure (+5) <*> s)
        `shouldBe` (s & each +~ 5)
