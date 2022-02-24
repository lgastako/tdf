{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TensorTest
  ( prop_mapRoundTrip
  , spec_Tensor
  ) where

import Protolude

import Orphans ()

import Test.Tasty.Hspec

import qualified Data.Map.Strict as Map

import qualified Data.Tensor as F

data B = F | T
  deriving (Eq, Generic, Ord, Show)

prop_mapRoundTrip :: Map Bool Bool -> Bool
prop_mapRoundTrip m = (m & F.toMap   . F.fromMap) == m
                   && (m'& F.fromMap . F.toMap  ) == m'
  where
    m' = F.fromMap m

spec_Tensor :: Spec
spec_Tensor = do
  context "given an empty 1-d tensor of B's indexed by Ints" $ do
    let series :: F.Tensor Int B
        series = F.empty

    it "should have length of 0" $
      length series `shouldBe` 0

  context "given a 1-d tensor of [T, T, F] indexed by Ints" $ do
    let series :: F.Tensor Int B
        series = F.fromMap . Map.fromList $
          [ (0, T)
          , (1, T)
          , (2, F)
          ]

    it "should have a length of 3" $
      length series `shouldBe` 3

    it "should be convertible to a list" $
      toList series `shouldBe` [T, T, F]

  context "given a 2-d tensor of [[T, F], [F, T]]" $ do
    let frame :: F.Tensor (Int, Int) B
        frame = F.fromMap . Map.fromList $
          [ ((0, 0), T)
          , ((1, 0), F)
          , ((0, 1), F)
          , ((1, 1), T)
          ]

    it "should have length 2" $
      length frame `shouldBe` 4

    -- TODO  Hmmmm... is this what we want? Probably yes, so that we
    --       can have the free generalized toList, but then we also
    --       need a toLists that returns an appropriately nested set
    --       of lists.
    it "should be convertible to a list" $
      toList frame `shouldBe` [T, F, F, T]

