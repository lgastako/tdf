{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RangeIndexTest where

import           TDF.Prelude

import           Test.Tasty.Hspec

import           TDF.Types.RangeIndex               ( stepping
                                                    , through
                                                    , upTo
                                                    )
import           TDF.Types.RangeIndex               ( RangeIndex )
import qualified TDF.Types.RangeIndex as RangeIndex

-- Examples from
-- https://cs.stanford.edu/people/nick/py/python-range.html
-- some of the examples seem to be wrong so I've checked them against python

data RangeIndexExpectations idx = RangeIndexExpectations
  { label          :: Text
  , expectedToList :: [idx]
  , testIndex      :: RangeIndex idx
  }

type IntExpectations = RangeIndexExpectations Int

spec_RangeIndex :: Spec
spec_RangeIndex = do
  let testExpectations RangeIndexExpectations {..} = do
        let _ = expectedToList :: [Int]
        context (cs label) $ do
          it "toList" $
            RangeIndex.toList testIndex `shouldBe` expectedToList
          it "length" $
            RangeIndex.length testIndex `shouldBe` length expectedToList

  context "constructors" $ do

    context "upTo" $ do

      testExpectations $ RangeIndexExpectations
        { label          = "upTo(0)"
        , expectedToList = []
        , testIndex      = upTo 0
        }

      testExpectations $ RangeIndexExpectations
        { label          = "upTo(1)"
        , expectedToList = [0]
        , testIndex      = upTo 1
        }

      testExpectations $ RangeIndexExpectations
        { label          = "upTo(5)"
        , expectedToList = [0, 1, 2, 3, 4]
        , testIndex      = upTo 5
        }

      testExpectations $ RangeIndexExpectations
        { label          = "negative number"
        , expectedToList = []
        , testIndex      = upTo (-42)
        }

  context "through" $ do

    testExpectations $ RangeIndexExpectations
      { label          = "3,5"
      , expectedToList = [3, 4]
      , testIndex      = 3 `through` 5
      }

  context "stepping" $ do

    testExpectations $ RangeIndexExpectations
      { label          = "0 3 1"
      , expectedToList = [0, 1, 2]
      , testIndex      = stepping 0 3 1
      }

    testExpectations $ RangeIndexExpectations
      { label          = "10 5 -1"
      , expectedToList = [10, 9, 8, 7, 6]
      , testIndex      = stepping 10 5 (-1)
      }

    testExpectations $ RangeIndexExpectations
      { label          = "6 5 -2"
      , expectedToList = [6]
      , testIndex      = stepping 6 5 (-2)
      }

    testExpectations $ RangeIndexExpectations
      { label          = "5 5 -2 (equal to stop is omitted)."
      , expectedToList = []
      , testIndex      = stepping 5 5 (-2)
      }

    testExpectations $ RangeIndexExpectations
      { label          = "4 5 -2 (beyond stop is omitted)."
      , expectedToList = []
      , testIndex      = stepping 4 5 (-2)
      }

  context "defaultFor" $ do

    it "should produce empty for empty input" $
      RangeIndex.defaultFor []
       `shouldBe`
         RangeIndex.empty

  context "defaultFromFor" $ do

    it "should produce empty for empty input" $
      RangeIndex.defaultFromFor 5 []
       `shouldBe`
         RangeIndex.RangeIndex 5 0 1 Nothing
