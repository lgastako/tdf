{-# LANGUAGE NoImplicitPrelude #-}

module VecTest where

import           TDF.Prelude

-- import           GHC.Natural            ( Natural )
import           Test.Tasty.Hspec

-- import qualified Data.Fin        as Fin
-- import qualified Data.Nat        as Nat
-- import           Data.Type.Nat          ( SNat )

import qualified Data.Vec.Lazy.X as Vec

spec_VecX :: Spec
spec_VecX = do
  context "given [1,2,3]" $ do
    let rEven :: Vec Nat1 Int
        rOdd  :: Vec Nat2 Int
        v     :: Vec Nat3 Int
        Just v     = Vec.fromList [1,2,3]
        Just rEven = Vec.fromList [2]
        Just rOdd  = Vec.fromList [1, 3]

    it "should find one even" $ do
      Vec.recoverVec (Vec.filter even v)
        `shouldBe` Just rEven

    it "should find two odd" $
      Vec.recoverVec (Vec.filter odd v)
        `shouldBe` Just rOdd

    it "should find three under 10" $
      Vec.recoverVec (Vec.filter (<10) v)
        `shouldBe` Just v

    it "should find none over 10" $
      Vec.recoverVec (Vec.filter (>10) v)
        `shouldBe` Just Vec.empty
