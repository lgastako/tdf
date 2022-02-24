{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid
  ( Frame
  , Series
  , frameToTexts
  , seriesToTexts
  ) where

import Data.Grid.Prelude

import Data.Grid.Series ( Series )
import Data.Renderable  ( Renderable( render ) )

newtype Frame r ri c ci a = Frame (Series r ri (Series c ci a))
  deriving (Eq, Ord, Show)

instance ( Universal a
         , Universal ri
         , Universal ci
         ) => Universal (Frame r ri c ci a)

frameToTexts :: forall r ri c ci a.
                Renderable a
             => Frame r ri c ci a
             -> [[Text]]
frameToTexts (Frame ss) = (fmap . fmap) render . toList . map toList $ ss

seriesToTexts :: forall n ix a.
                 Show a
              => Series n ix a
              -> [[Text]]
seriesToTexts = toList . map (pure . show)

-- getAVector :: IO (U.Vector Int)
-- getAVector = undefined

-- main :: IO ()
-- main = do
--     SomeSized v <- getAVector -- v is `Sized.Vector n Int`
--     -- get n in scope
--     SomeSized (v :: V.Vector n Int) <- getAVector
--     print v
