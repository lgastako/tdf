{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module IndexTest where

import           TDF.Prelude

import           Orphans                   ()
import           Test.Tasty.Hspec
import qualified TDF.Index        as Index
import           TDF.Index                 ( Index )

spec_Index :: Spec
spec_Index = do
  context "with defaultIntsFor 'bar'" $ do
    let idx :: Index Nat3 Int
        Just idx = Index.defaultIntsFor ("bar" :: FilePath)

    it "should have 0, 1, 2" $
      toList idx
        `shouldBe` [0, 1, 2]

    it "should append to itself correctly" $
      toList (Index.append idx idx)
        `shouldBe` [0, 1, 2, 3, 4, 5]
