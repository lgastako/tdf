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
                                                , Getter
                                                , Iso
                                                , Iso'
                                                , Lens
                                                , Lens'
                                                , Prism
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
import Data.Finite                  as X        ( Finite )
import Data.Frame.Prelude           as X        ( (...)
                                                , (|>)
                                                , cs
                                                , onCrash
                                                , orCrash
                                                )
import Data.Generics.Product.Fields as X        ( field )
-- I think maybe itt's better if we "force" it to be imported qualifed
--import Data.Vector.Sized            as X        ( Vector )
import GHC.TypeNats                 as X        ( type (+)
                                                , natVal'
                                                )
import Data.Generics.Product.Typed  as X        ( typed )


class (Eq a, Ord a, Show a) => Universal a

addToEnum :: Enum k => Int -> k -> k
addToEnum n = toEnum . (+n) . fromEnum
