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
    let v :: Vec Nat3 Int
        Just v = Vec.fromList [1,2,3]
        rEven :: Vec Nat1 Int
        Just rEven = Vec.fromList [2]
        rOdd :: Vec Nat2 Int
        Just rOdd = Vec.fromList [1, 3]

    it "should find one even" $ do
      Vec.recoverVec (snd $ Vec.filter even v)
        `shouldBe` Just rEven

    it "should find two odd" $
      Vec.recoverVec (snd $ Vec.filter odd v)
        `shouldBe` Just rOdd

    it "should find three under 10" $
      Vec.recoverVec (snd $ Vec.filter (<10) v)
        `shouldBe` Just v

    it "should find none over 10" $
      Vec.recoverVec (snd $ Vec.filter (>10) v)
        `shouldBe` Just Vec.empty

    -- it "should be possible to use it without knowing the length a priori" $ do
    --   let (len, v') = Vec.filter odd v
    --       finMay :: SNatI n => Maybe (Fin n)
    --       finMay = Fin.fromNat (Nat.fromNatural len)

    --       lenNat :: Nat
    --       lenNat = Nat.fromNatural len

    --   show (v' :: Vec n0 Int)
    --     `shouldBe` ("fixme" :: Text)



    -- TODO demonstrate pulling it out without knowing it in advance
