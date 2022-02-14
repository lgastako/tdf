{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module TDF.Prelude
  ( module X
  , (...)
  , cs
  ) where

import Protolude            as X hiding ( Map
                                        , Nat
                                        )

import Control.Arrow        as X ( (>>>) )
import Control.Lens         as X ( Lens'
                                 , each
                                 , lens
                                 , set
                                 , view
                                 , (%~)
                                 , (*~)
                                 , (+~)
                                 , (-~)
                                 , (.~)
                                 , (//~)
                                 , (?~)
                                 , (^.)
                                 )
import Data.Fin             as X ( Fin )
import Data.Generics.Labels      ()
import Data.Row             as X ( Disjoint
                                 , Empty
                                 , Forall
                                 , Label
                                 , Rec
                                 , type (.!)
                                 , type (.+)
                                 , type (.-)
                                 , type (.==)
                                 , type (≈)
                                 , (.!)
                                 , (.+)
                                 , (.-)
                                 , (.==)
                                 )
import Data.Row.Internal    as X ( Unconstrained1 )
import Data.Row.Records     as X ( Map
                                 , NativeRow
                                 , ToNative
                                 )
import Data.Type.Nat        as X ( Nat( S
                                      , Z
                                      )
                                 , Nat0
                                 , Nat1
                                 , Nat2
                                 , Nat3
                                 , Nat4
                                 , Nat5
                                 , Nat6
                                 , Nat7
                                 , Nat8
                                 , Nat9
                                 , Plus
                                 , SNatI
                                 )
import Data.Type.Nat.LE     as X ( LE )
import Data.Vec.Lazy        as X ( Vec( (:::)
                                      , VNil
                                      )
                                 )
import Data.Vector          as X ( Vector )
import Protolude.Conv       as X ( StringConv )
import Protolude.Conv            ( Leniency( Lenient )
                                 , strConv
                                 )

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)

cs :: StringConv a b => a -> b
cs = strConv Lenient
