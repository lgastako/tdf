{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module IndexTest
  ( spec_Index
  ) where

import Data.Frame.Prelude

import Data.Frame.Typed.Index ( Index )
import Orphans                ()
import Test.Tasty.Hspec       ( Spec
                              , context
                              , it
                              , shouldBe
                              )

import qualified Data.Frame.Typed.Index    as Index
import qualified Data.Frame.Typed.SubIndex as SubIndex
import qualified Data.Vec.Lazy.X           as Vec

spec_Index :: Spec
spec_Index =
  context "with defaultFor 'bar'" $ do
    let idx :: Index Nat3 Int
        idx = Index.defaultFor (Vec.repeat True)

    it "should have 0, 1, 2" $
      SubIndex.toLst idx
        `shouldBe` [0, 1, 2]

    it "should concat to itself correctly" $
      SubIndex.toLst (Index.concat idx idx)
        `shouldBe` [0, 1, 2, 3, 4, 5]
