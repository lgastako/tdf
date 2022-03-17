{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Relativ.Frame.Unsized
  ( Frame
  , fromSeries
  ) where

import Relativ.Frame.Prelude hiding ( Map )

import Data.Row.Records       ( Map )
import Relativ.Series.Unsized ( Series )

import qualified Data.Row.Records as Rec

newtype Frame (r :: Row *) = Frame (Rec (Map Series r))

deriving instance (Forall (Map Series r) Eq)   => Eq (Frame r)
deriving instance ( Forall (Map Series r) Eq
                  , Forall (Map Series r) Ord
                  )  => Ord (Frame r)
deriving instance (Forall (Map Series r) Show) => Show (Frame r)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromSeries :: forall r.
              ( Forall r Unconstrained1 )
           => Series (Rec r)
           -> Frame r
fromSeries = Frame . Rec.distribute

-- ================================================================ --
--   Optics
-- ================================================================ --

-- ================================================================ --
--   Combinators
-- ================================================================ --

-- ================================================================ --
--   Eliminators
-- ================================================================ --
