{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Series
  ( Options(..)
  , Series
  , construct
  , display
  ) where

import           TDF.Prelude

import qualified Data.Vector as Vector

-- See https://pandas.pydata.org/docs/reference/api/pandas.Series.html

data Series idx a = Series
  { sIndex  :: SA.Index idx
  , sData   :: Vector a
  , sLength :: Int
  , sName   :: Maybe Text
  } deriving (Eq, Ord, Generic, Show)

instance (NFData idx, NFData a) => NFData (Series idx a)

data Options idx a = Options
  { optIndex :: SA.Index idx
  , optData  :: Vector a
  , optName  :: Maybe Text
  } deriving (Generic)

deriving instance (Show a, Show idx) => Show (Options idx a)

-- ================================================================ --
--   Constructors
-- ================================================================ --

construct :: forall idx a. Options idx a -> Series idx a
construct Options {..} = Series
  { sIndex  = optIndex
  , sData   = optData
  , sLength = Vector.length optData
  , sName   = optName
  }

-- ================================================================ --
--   Combinators
-- ================================================================ --

-- ================================================================ --
--   Eliminators
-- ================================================================ --

display :: (Show idx, Show a) => Series idx a -> IO ()
display = print -- for now

-- ================================================================ --
--   Helpers / Temp
-- ================================================================ --

_s1 :: Series Int Int
_s1 = construct (Options (SA.defaultFor v) v Nothing)
  where
    v = Vector.fromList [10, 20, 30]
