{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Data.Grid.Prelude
  ( module X
  , Universal
  ) where

import Protolude               as X hiding ( from
                                           , to
                                           )
import Control.Monad.Primitive as X        ( PrimMonad
                                           , PrimState
                                           )
import Data.Frame.Prelude      as X        ( (...)
                                           , (|>)
                                           , cs
                                           )
import Data.Vector.Sized       as X        ( Vector )
import GHC.TypeNats            as X        ( type (+) )

class (Eq a, Ord a, Show a) => Universal a
