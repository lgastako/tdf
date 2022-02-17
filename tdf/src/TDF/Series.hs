{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module TDF.Series
  ( ASeries(..)
  , Options(..)
  , Series
  -- Constructors
  , a_
  , construct
  , empty
  , fromList
  , fromScalar
  , fromVec
  -- Combinators
  , append
  , drop
  , dropNaNs
  , duplicated
  , normalize
  , op
  , reverse
  , standardize
  , standardizeWith
  , t
  , take
  , updateVec
  -- Optics
  , at
  , name
  -- Eliminators
  , display
  , filter
  , filterThen
  , filterByIndex
  , filterByIndexThen
  , filterWithIndex
  , filterWithIndexThen
  , hasNaNs
  , index
  , isEmpty
  , ncols
  , ndims
  , nrows
  , onVec
  , reify
  , shape
  , toIndexedList
  , toList
  , toTexts
  , toVec
  , unique
  ) where

import           TDF.Prelude           hiding ( drop
                                              , empty
                                              , filter
                                              , reverse
                                              , take
                                              , toList
                                              )

import           Control.Lens                 ( Each )
import qualified Data.List          as List
import qualified Data.Map.Strict    as Map
import qualified Data.Vec.Lazy.X    as Vec
import qualified Data.Vec.Lazy.Lens as VL
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

data ASeries idx a = forall n. SNatI n => ASeries
  { size :: SNat n
  , vec  :: Series n idx a
  }

deriving instance Foldable    (ASeries idx)
deriving instance Functor     (ASeries idx)
deriving instance Traversable (ASeries idx)

-- instance Applicative (ASeries idx) where
--   pure :: forall a. a -> ASeries idx a
--   pure x = ASeries sn a
--     where
--       sn :: SNatI n => SNat n
--       sn = panic "sn undefined"
--       -- (snat @(FromGHC 1)) x

--       a :: Series n idx a
--       a = undefined

--   asf <*> asx = undefined

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

instance ToVecN (Series n idx a) n a where
  toVecN = toVec

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

-- | A synonym for `reify` for those who crave brevity.
a_ :: forall  idx a    b.
      (forall n. SNatI n => Series n idx a -> b)
   -> (ASeries  idx a -> b)
a_ = reify

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

fromScalar :: forall n idx a.
              ( SNatI n
              , idx ~ Int
              )
           => a
           -> Series n idx a
fromScalar x = Series
  { sIndex  = Index.defaultIntsFor sData' & orCrash "Series.fromScalar"
  , sData   = sData'
  , sLength = length sData'
  , sName   = Nothing
  }
  where
    sData' :: Vec n a
    sData' = Vec.repeat x

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

duplicated :: forall n idx a.
              ( Ord a )
           => Series n idx a
           -> Series n idx Bool
duplicated s = map ((>1) . (counts Map.!)) s
  where
    counts :: Map.Map a Int
    counts = Map.fromListWith (+) . map (, 1) . toList $ s

empty :: forall idx a. ( idx ~ Int )
      => Series Nat0 idx a
empty = Series
  { sIndex  = Index.defaultIntsFor sData' & orCrash "Series.empty"
  , sData   = sData'
  , sLength = length sData'
  , sName   = Nothing
  }
  where
    sData' = Vec.empty

filter :: forall n idx a.
          ( SNatI n
          , idx ~ Int
          )
       => (a -> Bool)
       -> Series n idx a
       -> ASeries idx a
filter p = filterWithIndex (\(_, x) -> p x)

filterThen :: forall n idx a r.
              ( SNatI n
              , idx ~ Int
              )
           => (a -> Bool)
           -> Series n idx a
           -> (ASeries idx a -> r)
           -> r
filterThen p s c = c . filter p $ s

filterByIndex :: forall n idx a.
                 ( SNatI n
                 , idx ~ Int
                 )
              => (idx -> Bool)
              -> Series n idx a
              -> ASeries idx a
filterByIndex p = filterWithIndex (\(idx, _) -> p idx)

filterByIndexThen :: forall n idx a r.
                     ( SNatI n
                     , idx ~ Int
                     )
                  => (idx -> Bool)
                  -> Series n idx a
                  -> (ASeries idx a -> r)
                  -> r
filterByIndexThen p s c = c . filterByIndex p $ s

filterWithIndex :: forall n idx a.
                   ( SNatI n
                   , idx ~ Int
                   )
                => ((idx, a) -> Bool)
                -> Series n idx a
                -> ASeries idx a
filterWithIndex p s@Series {..} = result
  where
    -- _ = p :: (idx, a) -> Bool
    -- _ = s :: Series n idx a

    v :: Vec n a
    v = toVec s

    indexed :: Vec n (idx, a)
    indexed = Vec.zip (Index.toVec sIndex) v

    aresult :: AVec (idx, a)
    aresult = Vec.filter p indexed

    aresult' :: AVec a
    aresult' = map snd aresult

    result :: ASeries idx a
    result = Vec.reify (mkASeries sName) aresult'

filterWithIndexThen :: forall n idx a r.
                       ( SNatI n
                       , idx ~ Int
                       )
                    => ((idx, a) -> Bool)
                    -> Series n idx a
                    -> (ASeries idx a -> r)
                    -> r
filterWithIndexThen p s c = c . filterWithIndex p $ s

mkASeries :: forall n idx a.
             ( SNatI n
             , idx ~ Int
             )
          => Maybe Text
          -> Vec n a
          -> ASeries idx a
mkASeries sName v = ASeries (snat @n) $ Series
  { sIndex  = Index.defaultIntsFor v & orCrash "f.Sindex"
  , sData   = v
  , sLength = Vec.length v
  , sName   = sName
  }

normalize :: forall n idx a.
             ( Fractional a
             , Ord a
             )
          => Series n idx a
          -> Series n idx a
normalize s = map f s
  where
    f x = (x - mn) / (mx - mn)

    mn = minimum s
    mx = maximum s

-- | Z-Score normalization
standardize :: forall n idx a.
               ( Floating a
               , Fractional a
               )
            => Series n idx a
            -> Series n idx a
standardize s = map f s
  where
    f x = (x - mu) / sd

    sd = stdDev s
    mu = mean s

-- | Z-Score normalization
standardizeWith :: forall n idx a.
                   ( Floating a
                   , Fractional a
                   )
                => a
                -> Series n idx a
                -> Series n idx a
standardizeWith mu s = map f s
  where
    f :: a -> a
    f x = (x - mu) / stdDev s

op :: ToVecN x n a
   => (a -> a -> b)
   -> x
   -> Series n idx a
   -> Series n idx b
op f x s = s { sData = Vec.zipWith f v v' }
  where
    v  = sData s
    v' = toVecN x

reverse :: Series n idx a -> Series n idx a
reverse = #sData %~ Vec.reverse

-- | The transpose of the Series, which according to Pandas, is the
-- series itself.
t :: Series n idx a -> Series n idx a
t = identity

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

name :: Lens' (Series n idx a) (Maybe Text)
name = lens get' set'
  where
    get' :: Series n idx a -> Maybe Text
    get' = sName

    set' :: Series n idx a
         -> Maybe Text
         -> Series n idx a
    set' s n = s { sName = n }

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

hasNaNs :: forall n idx a.
           ( RealFloat a )
        => Series n idx a
        -> Bool
hasNaNs = any isNaN

dropNaNs :: forall n idx a.
            ( RealFloat a
            , SNatI n
            , idx ~ Int
            )
         => Series n idx a
         -> ASeries idx a
dropNaNs = filter (not . isNaN)

index :: Series n idx a -> Index n idx
index = sIndex

isEmpty :: forall n idx a.
           ( SNatI n )
        => Series n idx a
        -> Bool
isEmpty _
  | snatToNat (snat @n) == nat0 = True
  | otherwise = False

ncols :: forall n idx a. Series n idx a -> Int
ncols _ = 1

ndims :: forall n idx a. Series n idx a -> Int
ndims _ = 1

nrows :: forall n idx a.
         ( SNatI n )
      => Series n idx a
      -> Int
nrows _ = fromIntegral . toInteger $ snatToNat (snat @n)

onVec :: forall n idx a b.
         (Vec n a -> b)
      -> Series n idx a
      -> b
onVec f Series {..} = f sData

reify :: forall idx a b.
         (forall n. SNatI n => Series n idx a -> b)
      -> ASeries idx a
      -> b
reify f (ASeries _n v) = f v

shape :: forall n idx a.
         ( Enum idx
         , SNatI n
         )
      => Series n idx a
      -> (Int, Int)
shape = (,) <$> nrows <*> ncols

toTextsVia :: forall n idx a. (a -> Text) -> Series n idx a -> [[Text]]
toTextsVia tt s = map pure . f . toVec $ s
  where
    f :: Vec n a -> [Text]
    f = (fromMaybe "series" (s ^. name):) . Vec.toList . Vec.map tt

toList :: Series n idx a -> [a]
toList = Vec.toList . toVec

toIndexedList :: Series n idx a -> [(idx, a)]
toIndexedList Series {..} = List.zip idxes vals
  where
    idxes = Index.toList sIndex
    vals  = Vec.toList sData

toTexts :: Show a => Series n idx a -> [[Text]]
toTexts = toTextsVia show

toVec :: Series n idx a -> Vec n a
toVec Series {..} = sData

-- TODO ifCtx for the (Ord a) with a fallback to Eq via compared nub to itself
unique :: forall n idx a.
          ( Ord a )
       => Series n idx a
       -> Bool
unique = not . any identity . duplicated
