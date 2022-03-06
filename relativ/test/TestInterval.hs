{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

module TestInterval
  ( spec_Interval
  ) where

import Relativ.Prelude

import Relativ.Types.Openness   ( Openness( Open
                                          , Closed
                                          , ClosedLeft
                                          , ClosedRight
                                          )
                                )
import Relativ.Types.Interval   ( Interval
                                , member
                                , notMember
                                )

import Test.Tasty.Hspec         ( Spec
                                , context
                                , hspec
                                , it
                                , shouldBe
                                )

import qualified Relativ.Types.Interval as Interval

_ = hspec

spec_Interval :: Spec
spec_Interval = do
  let shouldContain    iv x = x `member`    iv `shouldBe` True
      shouldNotContain iv x = x `notMember` iv `shouldBe` True

  context "given bounds (5, 1)" $ do
    let bounds :: (Int, Int)
        bounds = (5, 1)

    context "Closed" $ do
      let iv :: Interval 'Closed Int
          iv = Interval.build bounds

      it "should be empty" $ do
        Interval.empty iv `shouldBe` Just True

      it "should not contain anything" $
        all not (map (`member` iv) [-1..7]) `shouldBe` True

    context "Open" $ do
      let iv :: Interval 'Open Int
          iv = Interval.build bounds

      it "should be empty" $ do
        Interval.empty iv `shouldBe` Just True

      it "should not contain anything" $
        all not (map (`member` iv) [-1..7]) `shouldBe` True

    context "ClosedLeft" $ do
      let iv :: Interval 'ClosedLeft Int
          iv = Interval.build bounds

      it "should be empty" $ do
        Interval.empty iv `shouldBe` Just True

      it "should not contain anything" $
        all not (map (`member` iv) [-1..7]) `shouldBe` True

    context "ClosedRight" $ do
      let iv :: Interval 'ClosedRight Int
          iv = Interval.build bounds

      it "should be empty" $ do
        Interval.empty iv `shouldBe` Just True

      it "should not contain anything" $
        all not (map (`member` iv) [-1..7]) `shouldBe` True

  context "given bounds (1, 5)" $ do
    let bounds :: (Int, Int)
        bounds = (1, 5)

    context "Closed" $ do
      let iv :: Interval 'Closed Int
          iv = Interval.build bounds

      it "should contain 1" $
        iv `shouldContain` 1

      it "should contain 3" $
        iv `shouldContain` 3

      it "should contain 5" $
        iv `shouldContain` 5

      it "should not contain 0" $
        iv `shouldNotContain` 0

      it "should not contain 6" $
        iv `shouldNotContain` 6

    context "Open" $ do
      let iv :: Interval 'Open Int
          iv = Interval.build bounds

      it "should contain 3" $
        iv `shouldContain` 3

      it "should not contain 0" $
        iv `shouldNotContain` 0

      it "should not contain 1" $
        iv `shouldNotContain` 1

      it "should not contain 5" $
        iv `shouldNotContain` 5

      it "should not contain 6" $
        iv `shouldNotContain` 6

    context "ClosedLeft" $ do
      let iv :: Interval 'ClosedLeft Int
          iv = Interval.build bounds

      it "should contain 1" $
        iv `shouldContain` 1

      it "should contain 3" $
        iv `shouldContain` 3

      it "should not contain 0" $
        iv `shouldNotContain` 0

      it "should not contain 5" $
        iv `shouldNotContain` 5

      it "should not contain 6" $
        iv `shouldNotContain` 6

    context "ClosedRight" $ do
      let iv :: Interval 'ClosedRight Int
          iv = Interval.build bounds

      it "should contain 3" $
        iv `shouldContain` 3

      it "should contain 5" $
        iv `shouldContain` 5

      it "should not contain 0" $
        iv `shouldNotContain` 0

      it "should not contain 1" $
        iv `shouldNotContain` 1

      it "should not contain 6" $
        iv `shouldNotContain` 6
