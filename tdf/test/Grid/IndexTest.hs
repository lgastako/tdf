-- {-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications  #-}

module Grid.IndexTest
  -- ( spec_Index
  -- ) where
  where

-- import Data.Grid.Prelude

-- import Data.Grid.Index  ( Index )
-- import Test.Tasty.Hspec ( Spec
--                         , context
--                         , hspec
--                         , it
--                         , shouldBe
--                         )

-- import qualified Data.Grid.Index       as I
-- import qualified Data.Grid.Index.Class as C
-- import qualified Data.Vector.Sized     as Sized

-- _ = hspec

-- spec_Index :: Spec
-- spec_Index = do
--   context "with an empty unit index" $ do
--     let idx :: Index 0 Int
--         idx = I.empty

--     it "should toVector to an empty vector" $
--       C.toVector idx `shouldBe` Sized.empty @Int

--   context "with a 1-unit index" $ do
--     let idx :: Index 1 Int
--         idx = I.single ()

--     it "should toVector to the proper vector" $
--       C.toVector idx `shouldBe` Sized.single ()

--     -- it "should have length of 0" $
--     --   I.length idx `shouldBe` 0

--     -- it "should have an empty index" $
--     --   series ^. S.index
--     --     `shouldBe` I.empty

--     -- it "should toVector an empty Vector" $
--     --   Series.toVector series
--     --     `shouldBe` VectorIndex.empty
