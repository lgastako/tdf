{-# LANGUAGE NoImplicitPrelude #-}

module Relativ.Frame.Prelude
  ( module X
  , cs
  ) where

import Relativ.Prelude as X

import Data.Row          as X ( Row )
import Data.Row.Records  as X ( Forall
                              , Rec
                              )
import Data.Row.Internal as X ( Unconstrained1 )

import Protolude.Conv    as X ( Leniency( Lenient )
                              , StringConv
                              )

import Protolude.Conv         ( strConv )

cs :: StringConv a b => a -> b
cs = strConv Lenient
