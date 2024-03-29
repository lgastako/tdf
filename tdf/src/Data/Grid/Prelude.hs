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
import Control.Arrow                as X        ( (>>>) )
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
                                                , Each
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
                                                , preview
                                                , prism
                                                , review
                                                , set
                                                , to
                                                , view
                                                )
import Control.Monad.Primitive      as X        ( PrimMonad
                                                , PrimState
                                                )
import Data.Finite                  as X        ( Finite
                                                , getFinite
                                                , packFinite
                                                )
import Data.Frame.Prelude           as X        ( (...)
                                                , (|>)
                                                , cs
                                                , nl
                                                , onCrash
                                                , orCrash
                                                )
import Data.Generics.Product.Fields as X        ( field )
-- I think maybe itt's better if we "force" it to be imported qualifed
import Data.Vector.Sized.X          as X        ( ToVectorN( toVectorN ) )
import GHC.Natural                  as X        ( Natural )
import GHC.TypeNats                 as X        ( type (+)
                                                , natVal'
                                                )
import Data.Generics.Product.Typed  as X        ( typed )

class (Eq a, Ord a, Show a) => Universal a

addToEnum :: Enum k => Int -> k -> k
addToEnum n = toEnum . (+n) . fromEnum
