{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module TDF.Prelude
  ( module X
  , cs
  ) where

import Data.Row          as X        ( Disjoint
                                     , Empty
                                     , Forall
                                     , Label
                                     , KnownSymbol
                                     , Rec
                                     , type (.!)
                                     , type (.+)
                                     , type (.==)
                                     , type (≈)
                                     , (.!)
                                     , (.+)
                                     , (.==)
                                     )
import Data.Row.Internal as X        ( Unconstrained1 )
import Data.Row.Records  as X        ( Map
                                     , NativeRow
                                     , ToNative
                                     )
import Data.String       as X        ( String )
import Data.Vector       as X        ( Vector )
import Protolude         as X hiding ( Map )
import Protolude.Conv                ( Leniency( Lenient )
                                     , StringConv
                                     )

cs :: StringConv a b => a -> b
cs = strConv Lenient
