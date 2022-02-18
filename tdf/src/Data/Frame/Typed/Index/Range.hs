{-# LANGUAGE NoImplicitPrelude #-}

module Data.Frame.Typed.Index.Range
  ( RangeIndex(..)
  ) where

import Data.Frame.Prelude

data RangeIndex a = RangeIndex
  { start :: a
  , stop  :: a
  , step  :: a
  , name  :: Maybe Text
  }
