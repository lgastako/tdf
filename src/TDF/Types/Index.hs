{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module TDF.Types.Index
  ( Index(..)
  ) where

import TDF.Prelude

class (Bounded idx, Enum idx) => Index idx a where
  next  :: a -> idx -> idx
  start :: a -> idx
  stop  :: a -> idx
