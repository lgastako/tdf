{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module TDF.Series
  ( Options(..)
  , Series
  -- Constructors
  , construct
  , fromList
  , fromVec
  -- Combinators
  , append
  , drop
  , op
  , reverse
  , take
  , updateVec
  -- Optics
  , at
  -- Eliminators
  , display
  , filter
  , filterByIndex
  , filterWithIndex
  , index
  , onVec
  , toList
  , toTexts
  , toVec
  ) where

import           TDF.Prelude           hiding ( drop
                                              , filter
                                              , reverse
                                              , take
                                              , toList
                                              )

import           Control.Lens                 ( Each )
import qualified Data.List          as List
import qualified Data.Vec.Lazy.X    as Vec
import qualified Data.Vec.Lazy.Lens as VL
import qualified Data.Vector        as Vector
import           TDF.Index                    ( Index )
import qualified TDF.Index          as Index
import qualified TDF.Types.Table    as Table
import           TDF.Types.ToVecN             ( ToVecN( toVecN ) )

-- See https://pandas.pydata.org/docs/reference/api/pandas.Series.html

-- | One-dimensional series of data with axis labels
data Series (n :: Nat) idx a = Series
  { sIndex  :: Index n idx
  , sData   :: Vec n a
  , sLength :: Int
  , sName   :: Maybe Text
  } deriving (Eq, Foldable, Functor, Generic, Ord, Traversable, Show)

instance ( SNatI n
         , idx ~ Int
         ) => Applicative (Series n idx) where
  sf <*> sx = sf { sData = Vec.zipWith ($) (sData sf) (sData sx) }

  pure x = Series
    { sIndex  = Index.defaultIntsFor sData' & fromMaybe (panic "Series.pure")
    , sData   = sData'
    , sLength = Vec.length sData'
    , sName   = Nothing
    }
    where
      sData' = Vec.repeat x

instance (NFData idx, NFData a) => NFData (Series n idx a)

data Options (n :: Nat) idx a = Options
  { optIndex :: Index n idx
  , optData  :: Vec n a
  , optName  :: Maybe Text
  } deriving (Eq, Foldable, Functor, Generic, Ord, Traversable, Show)

instance Each (Series n idx a) (Series n idx a) a a

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

fromList :: forall n idx a.
            ( SNatI n
            , idx ~ Int
            )
         => [a]
         -> Maybe (Series n idx a)
fromList = fromVec <=< Vec.fromList

fromVec :: forall n idx a.
           ( SNatI n
           , idx ~ Int
           )
        => Vec n a
        -> Maybe (Series n idx a)
fromVec optData = f <$> Index.defaultIntsFor optData
  where
    f:: Index n idx -> Series n idx a
    f idx = Series
      { sIndex  = idx
      , sData   = optData
      , sLength = Vec.length optData
      , sName   = Nothing
      }

-- ================================================================ --
--   Combinators
-- ================================================================ --

-- TODO this way of appending indexes is proably wrong -- should instead do
-- what
-- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.append.html
-- does and only append rows that aren't in the target already (presumably
-- via idx)
append :: forall m n idx a.
          ( Num idx
          , Ord idx
          )
       => Series n idx a
       -> Series m idx a
       -> Series (Plus n m) idx a
append a b = Series
  { sIndex  = Index.append (sIndex a) (sIndex b)
  , sData   = (Vec.++) (sData a) (sData b)
  , sLength = sLength a + sLength b
  , sName   = sName a  -- TODO
  }

drop :: forall m n idx a.
        ( SNatI n
        , SNatI m
        , LE m n
        , idx ~ Int
        )
     => Series n idx a
     -> Series m idx a
drop s@Series {..} = s
  { sIndex = Index.defaultIntsFor sData' & orCrash "drop.sIndex"
  , sData  = sData'
  }
  where
    sData' :: Vec m a
    sData' = Vec.drop sData

filter :: (a -> Bool)
       -> Series n idx a
       -> Vector a
filter p = filterWithIndex (\(_, x) -> p x)

filterByIndex :: (idx -> Bool)
              -> Series n idx a
              -> Vector a
filterByIndex p = filterWithIndex (\(idx, _) -> p idx)

filterWithIndex :: forall n idx a.
                   ((idx, a) -> Bool)
                -> Series n idx a
                -> Vector a
filterWithIndex p s = Vector.map snd
                    . vecFilter p
                    . Vec.zipWith (,) (Index.toVec . sIndex $ s)
                    . toVec
                    $ s

op :: ToVecN x n a
   => (a -> a -> a)
   -> x
   -> Series n idx a
   -> Series n idx a
op f x s = s { sData = Vec.zipWith f v v' }
  where
    v  = sData s
    v' = toVecN x

reverse :: Series n idx a -> Series n idx a
reverse = #sData %~ Vec.reverse

take :: forall m n idx a.
        ( LE m n
        , SNatI m
        , SNatI n
        , idx ~ Int
        )
     => Series n idx a
     -> Series m Int a
take s@Series {..} = s
  { sIndex = Index.defaultIntsFor sData' & orCrash "drop.sIndex"
  , sData  = sData'
  }
  where
    sData' = Vec.take sData

updateVec :: forall m n idx a.
             ( SNatI n
             , SNatI m
             , idx ~ Int
             )
          => (Vec n a -> Vec m a)
          -> Series n idx a
          -> Series m idx a
updateVec f Series {..} = Series
  { sIndex  = sIndex'
  , sData   = sData'
  , sLength = Vec.length sData'
  , sName   = sName
  }
  where
    sData'  = f sData
    sIndex' = Index.defaultIntsFor sData'  -- TODO shouldn't do this...
      & orCrash "updateVec.sIindex"

vecFilter :: forall n a.
             (a -> Bool)
          -> Vec n a
          -> Vector a
vecFilter p = Vec.foldr f Vector.empty
  where
    f x acc | p x       = Vector.cons x acc
            | otherwise =               acc

-- ================================================================ --
--   Optics
-- ================================================================ --

-- TODO: Make it a prism instead of exploding.
--      TODO advanced mode: providing Indexing types that prohiibit
--           invalid access by contrusvtion
at :: forall n idx a.
      ( Eq idx
      , SNatI n
      )
   => idx
   -> Lens' (Series n idx a) a
at idx = lens get' set'
  where
    get' :: Series n idx a -> a
    get' s = view (#sData . VL.ix vecIdx) s
      where
        vecIdx :: Fin n
        vecIdx = Index.toFin idx (sIndex s)
          & fromMaybe (panic "Series.at.vecIdx.get' boom")

    set' :: Series n idx a -> a -> Series n idx a
    set' s x = set (#sData . VL.ix vecIdx) x s
      where
        vecIdx :: Fin n
        vecIdx = Index.toFin idx (sIndex s)
          & fromMaybe (panic "Series.at.vecIdx.set boom")

-- ================================================================ --
--   Eliminators
-- ================================================================ --

display :: (Show idx, Show a) => Series n idx a -> IO ()
display = putStr
  . Table.render
  . orCrash "Series.display"
  . Table.fromHeadedRows
  . List.map Table.Row
  . toTexts

index :: Series n idx a -> Index n idx
index = sIndex

onVec :: forall n idx a b.
         (Vec n a -> b)
      -> Series n idx a
      -> b
onVec f Series {..} = f sData

toTextsVia :: forall n idx a. (a -> Text) -> Series n idx a -> [[Text]]
toTextsVia tt = map pure . f . toVec
  where
    f :: Vec n a -> [Text]
    f = ("series":) . Vec.toList . Vec.map tt

toList :: Series n idx a -> [a]
toList = Vec.toList . toVec

toTexts :: Show a => Series n idx a -> [[Text]]
toTexts = toTextsVia show

toVec :: Series n idx a -> Vec n a
toVec Series {..} = sData
