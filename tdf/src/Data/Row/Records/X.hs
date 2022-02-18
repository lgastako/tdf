{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

module Data.Row.Records.X
  ( module Rec
  , fieldLabels
  ) where

import           TDF.Prelude

import           TDF.Types.ToField
import qualified Data.Row.Records  as Rec

fieldLabels :: Forall a ToField => Rec a -> [Text]
fieldLabels = Rec.erase @ToField @_ @Text toField