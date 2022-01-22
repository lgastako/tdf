{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Series
  ( Options
  , Series
  , construct
  ) where

import           TDF.Prelude

import qualified Data.Vector as Vector

-- See https://pandas.pydata.org/docs/reference/api/pandas.Series.html

data Series idx a = Series
  { sIndexes :: [idx]
  , sData    :: Vector a
  , sLength  :: Int
  , sName    :: Maybe Text
  } deriving (Eq, Ord, Generic, Show)

instance (NFData idx, NFData a) => NFData (Series idx a)

data Options idx a = Options
  { optIndexes :: [idx]
  , optData    :: Vector a
  , optName    :: Maybe Text
  } deriving (Generic)

deriving instance (Show a, Show idx) => Show (Options idx a)

construct :: forall idx a. Options idx a -> Series idx a
construct Options {..} = Series
  { sIndexes = optIndexes
  , sData    = optData
  , sLength  = Vector.length optData
  , sName    = optName
  }

_s1 :: Series Int Int
_s1 = construct (Options [0..3] (Vector.fromList [10,20,30]) Nothing)
