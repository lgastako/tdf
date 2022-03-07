{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestPeriod
  ( spec_Period
  ) where

import Relativ.Prelude

import Relativ.Types.Period ( Period )
import Test.Tasty.Hspec     ( Spec
                            , context
                            , hspec
                            , it
                            , runIO
                            , shouldBe
                            )

import qualified Relativ.Types.Period.Frequency as Frequency
import qualified Relativ.Types.Period           as Period

_ = hspec

spec_Period :: Spec
spec_Period = context "scalar/period/test_period.p" $ do
  context "test_construction" $ do
    context "chunk1" $ do
       -- i1 = Period("1/1/2005", freq="M")
      let i1 :: Period
          Just i1 = Period.fromFreqText Frequency.M "1/1/2005"

          -- i2 = Period("Jan 2005")
          i2 :: Period
          Just i2 = Period.fromText "Jan 2005"

      -- assert i1 == i2
      it "i1 should equal i2" $
        i1 == i2 `shouldBe` True
    context "chunk2" $ do
            -- i1 = Period("2005", freq="A")
      let Just i1 = Period.fromFreqText Frequency.A "2005"

            -- i2 = Period("2005")
          Just i2 = Period.fromText "2005"

            -- i3 = Period("2005", freq="a")
          Just i3 = Period.fromFreqText Frequency.A "2005"

            -- i4 = Period("2005", freq="M")
          Just i4 = Period.fromFreqText Frequency.M "2005"

          -- We can't do this in Haskell, and we're fine with that, so we'll
          -- just skip thhe tests related to i5.
            -- i5 = Period("2005", freq="m")
          -- Just i5 = Period.fromFreqText Frequency.m "2005"

      -- assert i1 == i2
      it "i1 should equal i2" $ i1 == i2 `shouldBe` True
      -- assert i1 == i3
      it "i1 should equal i3" $ i1 == i3 `shouldBe` True

      -- assert i1 != i4
      it "i1 should NOT equal i4" $ i1 == i4 `shouldBe` False

      -- assert i4 == i5
      -- Skipped
    context "chunk3" $ do
   -- i1  =          Period.now         ("Q")
      i1  <- runIO $ Period.now Frequency.Q
      now <- runIO $ getCurrentTime
      --       i2 = Period(datetime.now(), freq="Q")
      let i2 = Period.fromFreqTime Frequency.Q now
      -- i3 = Period.now("q")
      -- Skipped

      -- assert i1 == i2
      it "i1 shhould equal i2" $
        i1 == i2 `shouldBe` True

      -- assert i1 == i3
      -- skipped
