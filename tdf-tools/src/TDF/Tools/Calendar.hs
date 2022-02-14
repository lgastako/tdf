{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans     #-}

module TDF.Tools.Calendar where

import TDF.Prelude

import           Data.Time.Calendar       ( Day
                                          , DayOfMonth
                                          , MonthOfYear
                                          , Year
                                          , fromGregorian
                                          )
import           TDF.DataFrame            ( DataFrame )
import qualified TDF.DataFrame     as DF
import           TDF.Types.ToField        ( ToField( toField ) )

import           Data.Row.Records         ( Extend )

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
              -> DataFrame n idx r
              -> DataFrame n idx (Extend k Day r)
extendWithDay k f = DF.extendWith k g
  where
    g :: Rec r -> Day
    g = pack . f

    pack :: (Year, MonthOfYear, DayOfMonth) -> Day
    pack (y, m, d) = fromGregorian y m d
