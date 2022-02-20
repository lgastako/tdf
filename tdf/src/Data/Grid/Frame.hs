{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid.Frame
  ( Frame
  ) where

import Data.Grid.Prelude

import Data.Grid ( Series )

newtype Frame r c ri ci a = Frame (Series r ri (Series c ci a))
  deriving (Eq, Ord, Show)

instance ( Universal a
         , Universal ri
         , Universal ci
         ) => Universal (Frame r c ri ci a)
