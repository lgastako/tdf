{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TDF.Types.SimpleAbstractIndex
  ( Index
  ) where

import TDF.Prelude

import TDF.Types.RangeIndex ( RangeIndex )

data Index idx
  = ListIndex [idx]
  | RangeIndex (RangeIndex idx)
  deriving (Eq, Generic, Ord, Show)
