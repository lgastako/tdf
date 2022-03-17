{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Relativ.Frame.Sized
  ( Frame
  , fromSeries
  ) where

import Relativ.Frame.Prelude hiding ( Map )

import Data.Row.Records     ( Map )
import Relativ.Series.Sized ( Series )

import qualified Data.Row.Records as Rec

newtype Frame (n :: Nat) (r :: Row *) = Frame (Rec (Map (Series n) r))

deriving instance (Forall (Map (Series n) r) Eq)   => Eq (Frame n r)
deriving instance ( Forall (Map (Series n) r) Eq
                  , Forall (Map (Series n) r) Ord
                  )  => Ord (Frame n r)
deriving instance (Forall (Map (Series n) r) Show) => Show (Frame n r)

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromSeries :: forall n r.
              ( Forall r Unconstrained1 )
           => Series n (Rec r)
           -> Frame n r
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
