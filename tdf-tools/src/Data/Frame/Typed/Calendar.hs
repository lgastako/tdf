{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans     #-}

module Data.Frame.Typed.Calendar
  ( extendWithDay
  ) where

import Data.Frame.Prelude

import Data.Time.Calendar             ( Day
                                      , DayOfMonth
                                      , MonthOfYear
                                      , Year
                                      , fromGregorian
                                      )
import Data.Frame.Typed               ( Frame )
import Data.Frame.Typed.Types.ToField ( ToField( toField ) )

import qualified Data.Frame.Typed as DF

instance ToField Day where
  toField = show

extendWithDay :: forall n idx k r.
                 ( Forall r Unconstrained1
                 , Forall (Extend k Day r) Unconstrained1
                 , KnownSymbol k
                 , SNatI n
                 )
              => Label k
              -> (Rec r -> (Year, MonthOfYear, DayOfMonth))
              -> Frame n idx r
              -> Frame n idx (Extend k Day r)
extendWithDay k f = DF.extendWith k g
  where
    g :: Rec r -> Day
    g = pack . f

    pack :: (Year, MonthOfYear, DayOfMonth) -> Day
    pack (y, m, d) = fromGregorian y m d
