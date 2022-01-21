{-# LANGUAGE NoImplicitPrelude #-}

module TDF.Prelude
  ( module X
  , cs
  ) where

import Protolude as X
import Protolude.Conv ( Leniency( Lenient )
                      , StringConv
                      )

cs :: StringConv a b => a -> b
cs = strConv Lenient
