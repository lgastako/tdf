{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Data.Grid.Prelude
  ( module X
  , Universal
  , addToEnum
  ) where

import Protolude                    as X hiding ( from
                                                , to
                                                )
import Control.Lens                 as X        ( (%~)
                                                , (*~)
                                                , (+~)
                                                , (-~)
                                                , (.~)
                                                , (//~)
                                                , (?~)
                                                , (^.)
                                                , (^..)
                                                , (^?)
                                                , Iso'
                                                , Lens'
                                                , Prism'
                                                , _Just
                                                , _1
                                                , _2
                                                , ala
                                                , coerced
                                                , each
                                                , from
                                                , iso
                                                , ix
                                                , lens
                                                , prism
                                                , set
                                                , to
                                                , view
                                                )
import Control.Monad.Primitive      as X        ( PrimMonad
                                                , PrimState
                                                )
import Data.Data                    as X        ( Data )
import Data.Data.Lens               as X        ( biplate )
import Data.Frame.Prelude           as X        ( (...)
                                                , (|>)
                                                , cs
                                                , onCrash
                                                )
import Data.Generics.Product.Fields as X        ( field )
import Data.Vector.Sized            as X        ( Vector )
import GHC.TypeNats                 as X        ( type (+)
                                                , natVal'
                                                )
import Data.Generics.Product.Typed  as X        ( typed )


class (Eq a, Ord a, Show a) => Universal a

addToEnum :: Enum k => Int -> k -> k
addToEnum n = toEnum . (+n) . fromEnum
