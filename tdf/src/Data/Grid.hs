{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid
  ( Frame
  , Series
  ) where

import Data.Grid.Prelude

import Data.Grid.Series ( Series )

newtype Frame r ri c ci a = Frame (Series r ri (Series c ci a))
  deriving (Eq, Ord, Show)

instance ( Universal a
         , Universal ri
         , Universal ci
         ) => Universal (Frame r ri c ci a)

-- getAVector :: IO (U.Vector Int)
-- getAVector = undefined

-- main :: IO ()
-- main = do
--     SomeSized v <- getAVector -- v is `Sized.Vector n Int`
--     -- get n in scope
--     SomeSized (v :: V.Vector n Int) <- getAVector
--     print v
