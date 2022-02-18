{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Frame.Typed.Series
  ( ASeries(..)
  , Options(..)
  , Series
  -- Constructors
  , a_
  , afromList
  , construct
  , empty
  , fromList
  , fromScalar
  , fromVec
  , mkSeries
  , repeat
  -- Combinators
  , append
  , cons
  , drop
  , dropNaNs
  , duplicated
  , normalize
  , op
  , reverse
  , snoc
  , standardize
  , standardizeWith
  , t
  , take
  , updateVec
  , zip
  , zipWith
  -- Optics
  , at
  , dataVec
  , index
  , name
  -- Eliminators
  , aseries
  , display
  , filter
  , filterThen
  , filterByIndex
  , filterByIndexThen
  , filterWithIndex
  , filterWithIndexThen
  , hasNaNs
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

import           Data.Frame.Prelude             hiding ( drop
                                                       , empty
                                                       , filter
                                                       , repeat
                                                       , reverse
                                                       , take
                                                       , toList
                                                       , zip
                                                       , zipWith
                                                       )

import           Control.Lens                          ( Each )
import qualified Data.List                    as List
import qualified Data.Map.Strict              as Map
import qualified Data.Vec.Lazy.X              as Vec
import qualified Data.Vec.Lazy.AVec           as AVec
import qualified Data.Vec.Lazy.Lens           as VL
import           Data.Frame.Typed.Index                ( Index )
import qualified Data.Frame.Typed.Index       as Index
import qualified Data.Frame.Typed.Types.Table as Table
import           Data.Frame.Typed.Types.ToVecN         ( ToVecN( toVecN ) )

-- import qualified Control.Applicative as A

-- See https://pandas.pydata.org/docs/reference/api/pandas.Series.html

-- | One-dimensional series of data with axis labels
data Series (n :: Nat) idx a = Series
  { sIndex  :: Index n idx
  , sData   :: Vec n a
  , sLength :: Int
  , sName   :: Maybe Text
  } deriving (Eq, Foldable, Functor, Generic, Ord, Traversable, Show)

-- instance (SNatI n, idx ~ Int) => Alternative (Series n idx) where
--   empty :: forall a. Series n idx a
--   empty = undefined
--     where
--       x :: a
--       x = A.empty

--   (<|>) :: forall a.
--            Series n Int a
--         -> Series n Int a
--         -> Series n Int a
--   (<|>) = undefined

instance Semigroup a => Semigroup (Series n idx a) where
  s1 <> s2 = s1 & dataVec .~ s1 ^. dataVec
                          <> s2 ^. dataVec

instance (Monoid a, SNatI n, idx ~ Int) => Monoid (Series n idx a) where
  mempty = pure mempty

instance ( SNatI n
         , idx ~ Int
         ) => Applicative (Series n idx) where
  pure x = mkSeries Nothing (Vec.repeat x)
  sf <*> sx = sf { sData = Vec.zipWith ($) (sData sf) (sData sx) }

instance ( SNatI n
         , idx ~ Int
         ) => Monad (Series n idx) where
  (>>=) :: forall a b.
           Series n Int a
        -> (a -> Series n Int b)
        -> Series n Int b
  ma >>= mf = join' (fmap mf ma)

instance ToVecN (Series n idx a) n a where
  toVecN = toVec

instance (NFData idx, NFData a) => NFData (Series n idx a)

data ASeries idx a = forall n. SNatI n => ASeries
  { asSize   :: SNat n
  , asSeries :: Series n idx a
  }

aseries :: forall n idx a.
           ( SNatI n )
        => Series n idx a
        -> ASeries idx a
aseries = ASeries (snat @n)

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

-- filter :: forall n a. (a -> Bool) -> Vec n a -> AVec a
-- filter _ VNil = AVec SZ VNil
-- filter p (x ::: xs)
--   | p x, AVec _ v <- filter p xs = AVec SS (x ::: v)
--   | otherwise = filter p xs
afromList :: forall idx a.
             ( idx ~ Int )
          => [a]
          -> ASeries idx a
afromList [] = ASeries SZ empty
afromList (_x:_xs) = panic "afromList" -- cons x (afromList xs)

cons :: forall n idx a.
        ( SNatI n
        , idx ~ Int
        )
     => a
     -> Series n idx a
     -> Series (Plus Nat1 n) idx a
cons x = updateVec (Vec.cons x)

snoc :: forall n idx a.
        ( SNatI n
        , idx ~ Int
        )
     => a
     -> Series n idx a
     -> Series (Plus Nat1 n) idx a
snoc x = updateVec (`Vec.snoc` x)

fromScalar :: forall n idx a.
              ( SNatI n
              , idx ~ Int
              )
           => a
           -> Series n idx a
fromScalar = repeat

fromVec :: forall n idx a.
           ( SNatI n
           , idx ~ Int
           )
        => Vec n a
        -> Maybe (Series n idx a)
fromVec v = f <$> Index.defaultIntsFor v
  where
    f:: Index n idx -> Series n idx a
    f idx = Series
      { sIndex  = idx
      , sData   = v
      , sLength = Vec.length v
      , sName   = Nothing
      }

repeat :: forall n idx a.
          ( SNatI n
          , idx ~ Int
          )
       => a
       -> Series n idx a
repeat x = fromVec (Vec.repeat x) `onCrash` "Series.repeat"

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
  , sName   = sName a <> Just "_" <> sName b
  }

drop :: forall m n idx a.
        ( SNatI n
        , SNatI m
        , LE m n
        , idx ~ Int
        )
     => Series n idx a
     -> Series m idx a
drop = updateVec Vec.drop

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
empty = fromVec Vec.empty `onCrash` "Series.empty"

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
filterWithIndex p s@Series {..} = AVec.reify (mkASeries sName)
  . map snd
  . AVec.filter p
  . Vec.zip (Index.toVec sIndex)
  . toVec
  $ s

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
mkASeries n v = ASeries (snat @n) (mkSeries n v)

mkSeries :: SNatI n => Maybe Text -> Vec n a -> Series n Int a
mkSeries n v = fromVec v
  & orCrash "mkASeries.s"
  & name .~ n

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
    f x = (x - mu) / stdDev s

-- This implementation avoids the SNat (and idx ~ Int) constraints that would
-- be required if we used updateVec instead
op :: forall n idx a b x.
      ToVecN x n a
   => (a -> a -> b)
   -> x
   -> Series n idx a
   -> Series n idx b
op f x s = s { sData = Vec.zipWith f (sData s) (toVecN x) }

reverse :: forall n idx a.
           Series n idx a
        -> Series n idx a
reverse = dataVec %~ Vec.reverse

-- | The transpose of the Series, which according to Pandas, is the
-- series itself.
t :: forall n idx a. Series n idx a -> Series n idx a
t = identity

take :: forall m n idx a.
        ( LE m n
        , SNatI m
        , SNatI n
        , idx ~ Int
        )
     => Series n idx a
     -> Series m Int a
take = updateVec Vec.take

updateVec :: forall m n idx a b.
             ( SNatI m
             , idx ~ Int
             )
          => (Vec n a -> Vec m b)
          -> Series n idx a
          -> Series m idx b
updateVec f s = mkSeries (s ^. name) (f $ s ^. dataVec)

zip :: forall n idx a b.
       ( SNatI n
       , idx ~ Int
       )
    => Series n idx a
    -> Series n idx b
    -> Series n idx (a, b)
zip = zipWith (,)

zipWith :: forall n idx a b c.
           ( SNatI n
           , idx ~ Int
           )
        => (a -> b -> c)
        -> Series n idx a
        -> Series n idx b
        -> Series n idx c
zipWith f s1 s2 = updateVec (const $ Vec.zipWith f (sData s1) (sData s2)) s1

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
    get' s = view (dataVec . VL.ix vecIdx) s
      where
        vecIdx :: Fin n
        vecIdx = Index.toFin idx (sIndex s)
          & fromMaybe (panic "Series.at.vecIdx.get' boom")

    set' :: Series n idx a -> a -> Series n idx a
    set' s x = set (dataVec . VL.ix vecIdx) x s
      where
        vecIdx :: Fin n
        vecIdx = Index.toFin idx (sIndex s)
          & fromMaybe (panic "Series.at.vecIdx.set boom")

dataVec :: forall n idx a. Lens' (Series n idx a) (Vec n a)
dataVec = field @"sData"

name :: forall n idx a. Lens' (Series n idx a) (Maybe Text)
name = field @"sName"

-- ================================================================ --
--   Eliminators
-- ================================================================ --

display :: (Show idx, Show a) => Series n idx a -> IO ()
display = putStr
  . Table.render
  . (`onCrash` "Series.display")
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

index :: forall n idx a. Lens' (Series n idx a) (Index n idx)
index = field @"sIndex"

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
toVec = view dataVec

-- TODO ifCtx for the (Ord a) with a fallback to Eq via compared nub to itself
unique :: forall n idx a.
          ( Ord a )
       => Series n idx a
       -> Bool
unique = not . or . duplicated

-- ================================================================ --
--   Helpers
-- ================================================================ --

join' :: forall n idx a.
         ( SNatI n
         , idx ~ Int
         )
      => Series n idx (Series n idx a)
      -> Series n idx a
join' = updateVec (Vec.join . fmap toVec)
