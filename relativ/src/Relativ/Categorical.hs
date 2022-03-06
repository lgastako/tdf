{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Data.List     as L
import qualified Data.Vec.Lazy as Vec

-- | Index of `Categorical`s
data CategoricalIndex (c :: Nat) (n :: Nat) a = CategoricalIndex
  { categories  :: Vec c a
  , values      :: Vec n a
  } deriving (Eq, Functor, Ord, Show)

data BuildError
  = NoCategoriesOrValues
  deriving (Eq, Ord, Show)

instance Exception BuildError

build :: forall c n a.
         ( Eq a
         , SNatI c
         , SNatI n
         )
      => Maybe (Vec c a)
      -> Vec n a
      -> Either BuildError (CategoricalIndex c n a)
build catsMay vals = case catsMay of
  Just cats -> Right (CategoricalIndex cats vals)
  Nothing -> case Vec.fromList . L.nub . toList $ vals of
    Just cats -> Right (CategoricalIndex cats vals)
    Nothing -> Left NoCategoriesOrValues

build_ :: forall c n a.
          ( Eq a
          , SNatI c
          , SNatI n
          )
       => Maybe (Vec c a)
       -> Vec n a
       -> Maybe (CategoricalIndex c n a)
build_ = hush ... build

max :: forall c n a.
       Ord a
    => CategoricalIndex c n a
    -> a
max = maximum . values

min :: forall c n a.
       Ord a
    => CategoricalIndex c n a
    -> a
min = minimum . values
