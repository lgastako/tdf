{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Relativ.Categorical
  ( CategoricalIndex
  , build
  , build_
  , max
  , min
  ) where

import Relativ.Prelude hiding ( max
                              , min
                              )

import qualified Data.List.NonEmpty as NE

-- | Index of `Categorical`s
data CategoricalIndex a = CategoricalIndex
  { categories  :: NonEmpty a
  , values      :: NonEmpty a
  } deriving (Eq, Functor, Ord, Show)

data Ordered a = Ordered (Ord a => CategoricalIndex a)

deriving instance (Eq a, Ord a) => Eq (Ordered a)
deriving instance Ord a => Ord (Ordered a)
deriving instance (Ord a, Show a) => Show (Ordered a)

data BuildError
  = NoCategoriesOrValues
  deriving (Eq, Ord, Show)

instance Exception BuildError

build :: forall f g a.
         ( Eq a
         , Foldable f
         , Foldable g
         )
      => f a
      -> g a
      -> Either BuildError (CategoricalIndex a)
build cats vals = case NE.nonEmpty catsL of
  Nothing -> case NE.nonEmpty valsL of
    Nothing -> Left NoCategoriesOrValues
    Just valsN -> Right $ CategoricalIndex
      { categories  = categoriesFromValues valsN
      , values      = valsN
      }
  Just catsN -> case NE.nonEmpty valsL of
    Nothing -> Right $ CategoricalIndex
      { categories  = catsN
      , values      = panic "build.values" -- valuesFromCategories catsN
      }
    Just valsN -> Right $ CategoricalIndex
      { categories  = catsN
      , values      = valsN
      }
  where
    catsL :: [a]
    catsL = toList cats

    valsL :: [a]
    valsL = toList vals

build_ :: forall f g a.
          ( Eq a
          , Foldable f
          , Foldable g
          )
       => f a
       -> g a
       -> Maybe (CategoricalIndex a)
build_ = hush ... build

categoriesFromValues :: Eq a => NonEmpty a -> NonEmpty a
categoriesFromValues = NE.nub

max :: Ord a => CategoricalIndex a -> a
max = maximum . values

min :: Ord a => CategoricalIndex a -> a
min = minimum . values
