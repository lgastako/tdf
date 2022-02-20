{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Grid.SeriesTest
  ( spec_Series
  ) where

import Data.Grid.Prelude

import Data.Grid.Series ( Series )
import Test.Tasty.Hspec ( Spec
                        , context
                        , hspec
                        , it
                        , shouldBe
                        )

import qualified Data.Grid.Series   as Series

_ = hspec

spec_Series :: Spec
spec_Series = do
  context "with an empty series" $ do
    let series :: Series 0 Int ()
        series = Series.empty

    it "should have length of 0" $
      length series `shouldBe` 0

  context "with a unit series" $ do
    let series :: Series 1 Int ()
        series = Series.single ()

    it "should have a lengtht of 1" $
      length series `shouldBe` 1

  context "with a multi series" $ do
    let series :: Series 3 Int ()
        Just series = Series.fromList $ replicate 3 ()

    it "should have a lengtht of 1" $
      length series `shouldBe` 3

  -- context "with simple series" $ do
  --   let s :: Series Nat3 Int Float
  --       s = case Series.fromList [1.1, 2.2, 3.3] of
  --             Nothing -> panic "aaaaaah"
  --             Just  x -> x

  --       ss :: Series Nat6 Int Float
  --       ss = Series.concat s s

  --   it "should take5 properly" $
  --     Series.take5 (Series.concat s s)
  --       `shouldBe` (Series.take $ Series.concat s s)

  --   it "should drop5  properly" $
  --     Series.drop5 (Series.concat s s)
  --       `shouldBe` (Series.drop $ Series.concat s s)

  --   it "should self concat" $
  --     length ss `shouldBe` 6

  --   it "should filter" $
  --     Series.filterWithIndexThen ((<2.0) . snd) s toList
  --       `shouldBe` [1.1]

  --   it "should produce these exact indexes" $ do
  --     let ab :: Series Nat3 Int Bool
  --         ab = Series.concat a b

  --         a :: Series Nat1 Int Bool
  --         a = pure True

  --         b :: Series Nat2 Int Bool
  --         b = pure False

  --     (Vec.toList . SubIndex.toVec . view Series.index) ab
  --       `shouldBe` [0, 1, 2]

  --   it "should function applicatively" $
  --     (+) <$> s <*> pure (5 :: Float)
  --       `shouldBe` (s & each +~ 5)
