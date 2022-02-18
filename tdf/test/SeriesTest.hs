{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SeriesTest where

import           Data.Frame.Prelude

import           Data.Frame.Typed.Series               ( Series )
import qualified Data.Frame.Typed.Series   as Series
import qualified Data.Frame.Typed.SubIndex as SubIndex
import qualified Data.Vec.Lazy             as Vec
import           Orphans                               ()
import           Test.Tasty.Hspec

spec_Series :: Spec
spec_Series =
  context "with simple series" $ do
    let s :: Series Nat3 Int Float
        s = case Series.fromList [1.1, 2.2, 3.3] of
              Nothing -> panic "aaaaaah"
              Just  x -> x

        ss :: Series Nat6 Int Float
        ss = Series.concat s s

    it "should self concat" $
      length ss `shouldBe` 6

    it "should filter" $
      Series.filterWithIndexThen ((<2.0) . snd) s toList
        `shouldBe` [1.1]

    it "should produce these exact indexes" $ do
      let ab :: Series Nat3 Int Bool
          ab = Series.concat a b

          a :: Series Nat1 Int Bool
          a = pure True

          b :: Series Nat2 Int Bool
          b = pure False

      (Vec.toList . SubIndex.toVec . view Series.index) ab
        `shouldBe` [0, 1, 2]

    it "should function applicatively" $
      (+) <$> s <*> pure (5 :: Float)
        `shouldBe` (s & each +~ 5)
