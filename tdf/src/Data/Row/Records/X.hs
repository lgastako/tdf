{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

module Data.Row.Records.X
  ( module Rec
  , fieldLabels
  ) where

import           Data.Frame.Prelude

import           Data.Frame.Typed.ToField
import qualified Data.Row.Records         as Rec

fieldLabels :: Forall a ToField => Rec a -> [Text]
fieldLabels = Rec.erase @ToField @_ @Text toField
