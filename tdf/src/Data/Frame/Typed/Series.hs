{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
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
  , fake
  , fromList
  , fromNameAndVec
  , fromNamedVec
  , fromScalar
  , fromVec
  , mkSeries
  , repeat
  -- Combinators
  , concat
  , cons
  , drop
  , drop5
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
  , take5
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
  , nub
  , onVec
  , reify
  , shape
  , toIndexedList
  , toList
  , toTexts
  , toVec
  , unique
  ) where

import           Data.Frame.Prelude                 hiding ( concat
                                                           , drop
                                                           , empty
                                                           , filter
                                                           , repeat
                                                           , reverse
                                                           , take
                                                           , toList
                                                           , zip
                                                           , zipWith
                                                           )

import Control.Lens            ( Each )
import Data.Frame.Typed.Index  ( Index )
import Data.Frame.Typed.Name   ( Name )
import Data.Frame.Typed.ToVecN ( ToVecN( toVecN ) )
import Faker                   ( Fake )

import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import qualified Data.Vec.Lazy.X           as Vec
import qualified Data.Vec.Lazy.AVec        as AVec
import qualified Data.Vec.Lazy.Lens        as VL
import qualified Data.Frame.Typed.SubIndex as SubIndex
import qualified Data.Frame.Typed.Index    as Index
import qualified Data.Frame.Typed.Name     as Name
import qualified Data.Frame.Typed.Table    as Table
import qualified Faker
import qualified Faker.Combinators         as Fake

-- import qualified Control.Applicative as A

-- See https://pandas.pydata.org/docs/reference/api/pandas.Series.html

-- | One-dimensional series of data with axis labels
data Series (n :: Nat) idx a = Series
  { sIndex  :: Index n idx
  , sData   :: Vec n a
  , sName   :: Maybe Name
  } deriving (Eq, Foldable, Functor, Generic, Ord, Traversable, Show)

-- instance (Enum idx, SNatI n) => Alternative (Series n idx) where
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

instance ( Enum idx
         , Monoid a
         , SNatI n
         ) => Monoid (Series n idx a) where
  mempty = pure mempty

instance ( Enum idx
         , SNatI n
         ) => Applicative (Series n idx) where
  pure x = mkSeries Nothing (Vec.repeat x)
  sf <*> sx = sf { sData = Vec.zipWith ($) (sData sf) (sData sx) }

instance ( Enum idx
         , SNatI n
         ) => Monad (Series n idx) where
  (>>=) :: forall a b.
           Series n idx a
        -> (a -> Series n idx b)
        -> Series n idx b
  ma >>= mf = join' (fmap mf ma)

instance ToVecN (Series n idx a) n a where
  toVecN = toVec

-- instance (NFData idx, NFData a) => NFData (Series n idx a)

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
  , optName  :: Maybe Name
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

construct :: forall n idx a.
             Options n idx a
          -> Series n idx a
construct Options {..} = Series
  { sIndex  = optIndex
  , sData   = optData
  , sName   = optName
  }

fake :: forall n idx a.
        ( Enum idx
        , SNatI n
        )
     => Fake a
     -> IO (Series n idx a)
fake gen = Faker.generateNonDeterministic (Fake.listOf n gen)
  <&> fromVec . orCrash error . Vec.fromList @n
  where
    error = "The fake data that was generated was the wrong size."
    n     = fromIntegral $ snatToNat (snat @n)

fromList :: forall n idx a.
            ( Enum idx
            , SNatI n
            )
         => [a]
         -> Maybe (Series n idx a)
fromList = (Just <$> fromVec) <=< Vec.fromList -- TODO

-- filter :: forall n a. (a -> Bool) -> Vec n a -> AVec a
-- filter _ VNil = AVec SZ VNil
-- filter p (x ::: xs)
--   | p x, AVec _ v <- filter p xs = AVec SS (x ::: v)
--   | otherwise = filter p xs
afromList :: forall idx a.
             ( Enum idx )
          => [a]
          -> ASeries idx a
afromList [] = ASeries SZ empty
afromList (_x:_xs) = panic "afromList" -- cons x (afromList xs)

cons :: forall n idx a.
        ( Enum idx
        , SNatI n
        )
     => a
     -> Series n idx a
     -> Series (Plus Nat1 n) idx a
cons x = updateVec (Vec.cons x)

snoc :: forall n idx a.
        ( Enum idx
        , SNatI n
        )
     => a
     -> Series n idx a
     -> Series (Plus Nat1 n) idx a
snoc x = updateVec (`Vec.snoc` x)

fromScalar :: forall n idx a.
              ( Enum idx
              , SNatI n
              )
           => a
           -> Series n idx a
fromScalar = repeat

fromNameAndVec :: forall n idx a.
                  ( Enum idx
                  , SNatI n
                  )
               => Maybe Name
               -> Vec n a
               -> Series n idx a
fromNameAndVec nameMay v = Series
  { sIndex  = Index.defaultFromFor (toEnum 0) v
  , sData   = v
  , sName   = nameMay
  }

fromNamedVec :: forall n idx a.
                ( Enum idx
                , SNatI n
                )
             => Name
             -> Vec n a
             -> Series n idx a
fromNamedVec = fromNameAndVec . Just

fromVec :: forall n idx a.
           ( Enum idx
           , SNatI n
           )
        => Vec n a
        -> Series n idx a
fromVec = fromNameAndVec Nothing

repeat :: forall n idx a.
          ( Enum idx
          , SNatI n
          )
       => a
       -> Series n idx a
repeat x = fromVec (Vec.repeat x)

-- ================================================================ --
--   Combinators
-- ================================================================ --

-- TODO this way of appending indexes is proably wrong -- should instead do
-- what
-- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.append.html
-- does and only append rows that aren't in the target already (presumably
-- via idx)
concat :: forall m n idx a.
          ( Ord idx
          , SNatI n
          , SNatI m
          , SNatI (Plus n m)
          )
       => Series n idx a
       -> Series m idx a
       -> Series (Plus n m) idx a
concat a b = Series
  { sIndex  = Index.concat (a ^. index) (b ^. index)
  , sData   = a ^. dataVec Vec.++ b ^. dataVec
  , sName   = Name.combine (a ^. name) (b ^. name)
  }

drop :: forall m n idx a.
        ( Enum idx
        , LE m n
        , SNatI n
        , SNatI m
        )
     => Series n idx a
     -> Series m idx a
drop = updateVec Vec.drop

drop5 :: forall m n idx a.
         ( Enum idx
         , LE m n
         , SNatI n
         , SNatI m
         , m ~ Nat5
         )
      => Series n idx a
      -> Series m idx a
drop5 = drop

duplicated :: forall n idx a.
              ( Ord a )
           => Series n idx a
           -> Series n idx Bool
duplicated s = map ((>1) . (counts Map.!)) s
  where
    counts :: Map.Map a Int
    counts = Map.fromListWith (+) . map (, 1) . toList $ s

empty :: forall idx a. Enum idx
      => Series Nat0 idx a
empty = fromVec Vec.empty

filter :: forall n idx a.
          ( Enum idx
          , SNatI n
          )
       => (a -> Bool)
       -> Series n idx a
       -> ASeries idx a
filter p = filterWithIndex (\(_, x) -> p x)

filterThen :: forall n idx a r.
              ( Enum idx
              , SNatI n
              )
           => (a -> Bool)
           -> Series n idx a
           -> (ASeries idx a -> r)
           -> r
filterThen p s c = c . filter p $ s

filterByIndex :: forall n idx a.
                 ( Enum idx
                 , SNatI n
                 )
              => (idx -> Bool)
              -> Series n idx a
              -> ASeries idx a
filterByIndex p = filterWithIndex (\(idx, _) -> p idx)

filterByIndexThen :: forall n idx a r.
                     ( Enum idx
                     , SNatI n
                     )
                  => (idx -> Bool)
                  -> Series n idx a
                  -> (ASeries idx a -> r)
                  -> r
filterByIndexThen p s c = c . filterByIndex p $ s

filterWithIndex :: forall n idx a.
                   ( Enum idx
                   , SNatI n
                   )
                => ((idx, a) -> Bool)
                -> Series n idx a
                -> ASeries idx a
filterWithIndex p s@Series {..} = AVec.reify (mkASeries sName)
  . map snd
  . AVec.filter p
  . Vec.zip (SubIndex.toVec sIndex)
  . toVec
  $ s

filterWithIndexThen :: forall n idx a r.
                       ( Enum idx
                       , SNatI n
                       )
                    => ((idx, a) -> Bool)
                    -> Series n idx a
                    -> (ASeries idx a -> r)
                    -> r
filterWithIndexThen p s c = c . filterWithIndex p $ s

mkASeries :: forall n idx a.
             ( Enum idx
             , SNatI n
             )
          => Maybe Name
          -> Vec n a
          -> ASeries idx a
mkASeries n v = ASeries (snat @n) (mkSeries n v)

mkSeries :: forall n idx a.
            ( Enum idx
            , SNatI n
            )
         => Maybe Name
         -> Vec n a
         -> Series n idx a
mkSeries n v = fromVec v
  |> name .~ n

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

-- This implementation avoids the SNat constraint that would be required if we
-- used updateVec instead.  Don't know if there's any real value in that or
-- not.
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
        ( Enum idx
        , LE m n
        , SNatI m
        , SNatI n
        )
     => Series n idx a
     -> Series m idx a
take = updateVec Vec.take

take5 :: forall m n idx a.
         ( Enum idx
         , LE m n
         , SNatI m
         , SNatI n
         , m ~ Nat5
         )
     => Series n idx a
     -> Series m idx a
take5 = take

updateVec :: forall m n idx a b.
             ( Enum idx
             , SNatI m
             )
          => (Vec n a -> Vec m b)
          -> Series n idx a
          -> Series m idx b
updateVec f s = mkSeries (s ^. name) (f $ s ^. dataVec)

zip :: forall n idx a b.
       ( Enum idx
       , SNatI n
       )
    => Series n idx a
    -> Series n idx b
    -> Series n idx (a, b)
zip = zipWith (,)

zipWith :: forall n idx a b c.
           ( Enum idx
           , SNatI n
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
      ( Enum idx
      , Eq idx
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

name :: forall n idx a. Lens' (Series n idx a) (Maybe Name)
name = field @"sName"

-- ================================================================ --
--   Eliminators
-- ================================================================ --

display :: forall n idx a. (Show idx, Show a) => Series n idx a -> IO ()
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
            ( Enum idx
            , RealFloat a
            , SNatI n
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

-- TODO ifCxt for the (Ord a) with a fallback to Eq via compared nub to itself
unique :: forall n idx a.
          ( Ord a )
       => Series n idx a
       -> Bool
unique = not . or . duplicated

ncols :: forall n idx a. Series n idx a -> Int
ncols _ = 1

ndims :: forall n idx a. Series n idx a -> Int
ndims _ = 1

nrows :: forall n idx a.
         ( SNatI n )
      => Series n idx a
      -> Int
nrows _ = fromIntegral . toInteger $ snatToNat (snat @n)

-- TODO: ifCxt to use ordNub when possible
nub :: Eq a => Series n idx a -> [a]
nub = List.nub . toList

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
    f = (renderedName:) . Vec.toList . Vec.map tt

    renderedName :: Text
    renderedName = Name.unName $ fromMaybe defaultName (s ^. name)

    defaultName :: Name
    defaultName = Name.series

toList :: Series n idx a -> [a]
toList = Vec.toList . toVec

toIndexedList :: forall n idx a.
                 ( Enum idx
                 , SNatI n
                 )
              => Series n idx a
              -> [(idx, a)]
toIndexedList s = List.zip idxes vals
  where
    idxes = SubIndex.toLst $ s ^. index
    vals  = Vec.toList $ s ^. dataVec

toTexts :: Show a => Series n idx a -> [[Text]]
toTexts = toTextsVia show

toVec :: Series n idx a -> Vec n a
toVec = view dataVec

-- ================================================================ --
--   Helpers
-- ================================================================ --

join' :: forall n idx a.
         ( Enum idx
         , SNatI n
         )
      => Series n idx (Series n idx a)
      -> Series n idx a
join' = updateVec (Vec.join . fmap toVec)
