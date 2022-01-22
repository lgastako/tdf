{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TDF.Types.SAI
  ( Index
  , take
  ) where

import                   TDF.Prelude         hiding ( take )

import qualified Data.List            as List
import           TDF.Types.RangeIndex               ( RangeIndex )
import qualified TDF.Types.RangeIndex as RangeIndex

data Index idx
  = ListIndex [idx]
  | RangeIndex (RangeIndex idx)
  deriving (Eq, Generic, Ord, Show)

take :: Int -> Index idx -> Index idx
take n = \case
  ListIndex indexes -> ListIndex (List.take n indexes)
  RangeIndex ri -> RangeIndex $ RangeIndex.take n ri
