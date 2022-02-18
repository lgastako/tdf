{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module IndexTest where

import           Data.Frame.Prelude

import qualified Data.Frame.Typed.Index    as Index
import           Data.Frame.Typed.Index                ( Index )
import qualified Data.Frame.Typed.SubIndex as SubIndex
import qualified Data.Vec.Lazy.X           as Vec
import           Test.Tasty.Hspec
import           Orphans                               ()

spec_Index :: Spec
spec_Index =
  context "with defaultIntsFor 'bar'" $ do
    let idx :: Index Nat3 Int
        Just idx = Index.defaultIntsFor (Vec.repeat True)

    it "should have 0, 1, 2" $
      SubIndex.toLst idx
        `shouldBe` [0, 1, 2]

    it "should append to itself correctly" $
      SubIndex.toLst (Index.concat idx idx)
        `shouldBe` [0, 1, 2, 3, 4, 5]
