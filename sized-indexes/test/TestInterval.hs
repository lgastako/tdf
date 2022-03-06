{-# LANGUAGE NoImplicitPrelude #-}

module TestInterval
  ( spec_Interval
  ) where

import Relativ.Prelude

import Relativ.Types.Closedness ( Closedness( Open
                                            , Closed
                                            , ClosedLeft
                                            , ClosedRight
                                            )
                                )
import Relativ.Types.Interval   ( in_ )

import Test.Tasty.Hspec ( Spec
                        , context
                        , hspec
                        , it
                        , shouldBe
                        )

import qualified Relativ.Types.Interval as Interval

_ = hspec

spec_Interval :: Spec
spec_Interval = do
  let shouldContain    iv x = x `in_` iv `shouldBe` True
      shouldNotContain iv x = x `in_` iv `shouldBe` False

  context "given bounds (1, 5)" $ do
    let bounds :: (Int, Int)
        bounds = (1, 5)

    context "Closed" $ do
      let c = Closed
          Just iv = Interval.build c bounds

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
      let c = Open
          Just iv = Interval.build c bounds

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
      let c = ClosedLeft
          Just iv = Interval.build c bounds

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
      let c = ClosedRight
          Just iv = Interval.build c bounds

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

