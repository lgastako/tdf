{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relativ.Types.Flag
  ( Flag
  , toFlag
  , fromFlag
  ) where

import Relativ.Prelude

newtype Flag a = Flag Bool
  deriving (Eq, Ord, Show)

fromFlag :: forall a.
            a
         -> Flag a
         -> Bool
fromFlag _ (Flag x) = x

toFlag :: forall a.
          a
       -> Bool
       -> Flag a
toFlag _ = Flag
