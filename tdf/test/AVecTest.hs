{-# LANGUAGE NoImplicitPrelude #-}

module AVecTest
  ( spec_VecX
  ) where

import Data.Frame.Prelude

import Test.Tasty.Hspec

import qualified Data.Vec.Lazy.AVec as AVec
import qualified Data.Vec.Lazy.X    as Vec

spec_VecX :: Spec
spec_VecX =
  context "given [1,2,3]" $ do
    let rEven :: Vec Nat1 Int
        rOdd  :: Vec Nat2 Int
        v     :: Vec Nat3 Int
        Just v     = Vec.fromList [1,2,3]
        Just rEven = Vec.fromList [2]
        Just rOdd  = Vec.fromList [1, 3]

    it "should find one even" $
      AVec.recoverVec (AVec.filter even v)
        `shouldBe` Just rEven

    it "should find two odd" $
      AVec.recoverVec (AVec.filter odd v)
        `shouldBe` Just rOdd

    it "should find three under 10" $
      AVec.recoverVec (AVec.filter (<10) v)
        `shouldBe` Just v

    it "should find none over 10" $
      AVec.recoverVec (AVec.filter (>10) v)
        `shouldBe` Just Vec.empty
