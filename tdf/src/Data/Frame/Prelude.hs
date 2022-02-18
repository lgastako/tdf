{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Frame.Prelude
  ( module X
  , (...)
  , cs
  , explode
  , mean
  , nl
  , onCrash
  , orCrash
  , stdDev
  ) where

import Protolude             as X hiding ( Map
                                         , Nat
                                         )

import Control.Arrow         as X ( (>>>) )
import Control.Lens          as X ( Lens'
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
import Data.Fin              as X ( Fin )
import Data.Generics.Labels  as X ()
import Data.Generics.Product as X ( field
                                  , typed
                                  )
import Data.Row             as X ( AllUniqueLabels
                                 , Disjoint
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
import Data.Row.Records     as X ( Extend
                                 , Map
                                 , NativeRow
                                 , ToNative
                                 )
import Data.Type.Equality   as X ( testEquality )
import Data.Type.Nat        as X ( FromGHC
                                 , Nat( S
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
                                 , SNat( SS
                                       , SZ
                                       )
                                 , nat0
                                 , snat
                                 , snatToNat
                                 )
import Data.Type.Nat.LE     as X ( LE )
import Data.Vec.Lazy.X      as X ( Vec( (:::)
                                      , VNil
                                      )
                                 )
import Protolude.Conv       as X ( StringConv )
import Protolude.Conv            ( Leniency( Lenient )
                                 , strConv
                                 )

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)

cs :: StringConv a b => a -> b
cs = strConv Lenient

explode :: Text -> a
explode = panic . ("ERROR:EXPLOSION:" <>)

mean :: ( Foldable f
        , Fractional a
        )
     => f a
     -> a
mean = (/) <$> sum <*> realToFrac . length

nl :: IO ()
nl = putText ""

onCrash :: Maybe a -> Text -> a
onCrash = flip orCrash

orCrash :: Text -> Maybe a -> a
orCrash s = fromMaybe (panic s)

stdDev :: ( Floating a
          , Foldable f
          , Functor f
          , Fractional a
          ) => f a -> a
stdDev xs = sqrt . mean . map ((^ (2 :: Int)) . (-) (mean xs)) $ xs
