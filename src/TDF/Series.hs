{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Series
  ( Options(..)
  , Series
  , append
  , construct
  , display
  , index
  , fromList
  , fromVec
  , toVec
  ) where

import           TDF.Prelude

import qualified Data.List       as List
import qualified Data.Vec.Lazy   as Vec
import           TDF.Index                ( Index )
import qualified TDF.Index       as Idx
import qualified TDF.Types.Table as Table

-- See https://pandas.pydata.org/docs/reference/api/pandas.Series.html

-- | One-dimensional series of data with axis labels
data Series (n :: Nat) idx a = Series
  { sIndex  :: Index n idx
  , sData   :: Vec n a
  , sLength :: Int
  , sName   :: Maybe Text
  } deriving (Eq, Foldable, Functor, Generic, Ord, Traversable, Show)

instance (NFData idx, NFData a) => NFData (Series n idx a)

data Options (n :: Nat) idx a = Options
  { optIndex :: Index n idx
  , optData  :: Vec n a
  , optName  :: Maybe Text
  } deriving (Eq, Foldable, Functor, Generic, Ord, Traversable, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

construct :: forall n idx a. Options n idx a -> Series n idx a
construct Options {..} = Series
  { sIndex  = optIndex
  , sData   = optData
  , sLength = Vec.length optData
  , sName   = optName
  }

fromList :: forall n a. ( SNatI n )
         => [a]
         -> Maybe (Series n Int a)
fromList = fromVec <=< Vec.fromList

fromVec :: forall n a. ( SNatI n )
        => Vec n a
        -> Maybe (Series n Int a)
fromVec optData = f <$> Idx.defaultIntsFor optData
  where
    f:: Index n Int -> Series n Int a
    f idx = Series
      { sIndex  = idx
      , sData   = optData
      , sLength = Vec.length optData
      , sName   = Nothing
      }

-- ================================================================ --
--   Combinators
-- ================================================================ --

append :: forall m n idx a.
          ( Num idx
          , Ord idx
          )
       => Series n idx a
       -> Series m idx a
       -> Series (Plus n m) idx a
append a b = Series
    { sIndex  = Idx.append (sIndex a) (sIndex b)
    , sData   = (Vec.++) (sData a) (sData b)
    , sLength = sLength a + sLength b
    , sName   = sName a  -- TODO
    }

-- ================================================================ --
--   Eliminators
-- ================================================================ --

display :: (Show idx, Show a) => Series n idx a -> IO ()
display = putStr
  . Table.render
  . fromMaybe explode
  . Table.fromHeadedRows
  . List.map Table.Row
  . toTexts
  where
    explode = panic "display explode"

index :: Series n idx a -> Index n idx
index = sIndex

toTextsVia :: forall n idx a. (a -> Text) -> Series n idx a -> [[Text]]
toTextsVia tt = map pure . f . toVec
  where
    f :: Vec n a -> [Text]
    f = ("series":) . Vec.toList . Vec.map tt

toTexts :: Show a => Series n idx a -> [[Text]]
toTexts = toTextsVia show

toVec :: Series n idx a -> Vec n a
toVec Series {..} = sData

-- ================================================================ --
--   Helpers / Temp
-- ================================================================ --

_s1 :: Series Nat3 Int Int
_s1 = construct $ opts <$> Idx.defaultIntsFor v
  & fromMaybe (panic "invalid series size probably")
  where
    opts :: Index Nat3 Int -> Options Nat3 Int Int
    opts idx = Options idx v Nothing

    v :: Vec Nat3 Int
    v = fromMaybe (panic "_s1.1") . Vec.fromList $ [10, 20, 30]
