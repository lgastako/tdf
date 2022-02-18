{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module IndexTest where

import           Data.Frame.Prelude

import qualified Data.Frame.Typed.Index as Index
import           Data.Frame.Typed.Index          ( Index )
import           Test.Tasty.Hspec
import           Orphans                         ()

spec_Index :: Spec
spec_Index =
  context "with defaultIntsFor 'bar'" $ do
    let idx :: Index Nat3 Int
        Just idx = Index.defaultIntsFor ("bar" :: FilePath)

    it "should have 0, 1, 2" $
      toList idx
        `shouldBe` [0, 1, 2]

    it "should append to itself correctly" $
      toList (Index.append idx idx)
        `shouldBe` [0, 1, 2, 3, 4, 5]
