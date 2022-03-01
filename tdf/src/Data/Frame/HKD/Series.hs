{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Frame.HKD.Series
  ( Series
  ) where

import Data.Frame.HKD.Prelude

import Generics.OneLiner      ( Constraints )

import Data.Frame.Typed.Index ( Index )
import Data.Frame.Typed.Name  ( Name )


-- TODO: The HKD is "hidden" inside the type, so there's no way (that I can
-- think of at the moment) to have the Series type "insist" on being provided
-- an HKD laden data type.  And if there were, there's still no way to take
-- advantage of that fact.
data Person f = Person
  { pName :: HKD f Text
  , pAge  :: HKD f Int
  } deriving (Generic)

deriving instance Constraints (Person f) Eq   => Eq   (Person f)
deriving instance Constraints (Person f) Ord  => Ord  (Person f)
deriving instance Constraints (Person f) Show => Show (Person f)

-- | One-dimensional series of data with axis labels
data Series (n :: Nat) idx a = Series
  { sIndex  :: Index n idx
  , sData   :: Vec n a
  , sName   :: Maybe Name
  } deriving (Eq, Foldable, Functor, Generic, Ord, Traversable, Show)
