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

import qualified Data.List         as L
import qualified Data.Vector.Sized as S

-- | Index of `Categorical`s
data CategoricalIndex (c :: Nat) (n :: Nat) a = CategoricalIndex
  { categories  :: S.Vector c a
  , values      :: S.Vector n a
  } deriving (Eq, Functor, Ord, Show)

data BuildError
  = NoCategoriesOrValues
  deriving (Eq, Ord, Show)

instance Exception BuildError

build :: forall c n a.
         ( Eq a
         , KnownNat c
         , KnownNat n
         )
      => Maybe (S.Vector c a)
      -> S.Vector n a
      -> Either BuildError (CategoricalIndex c n a)
build catsMay vals = case catsMay of
  Just cats -> Right (CategoricalIndex cats vals)
  Nothing -> case S.fromList . L.nub . toList $ vals of
    Just cats -> Right (CategoricalIndex cats vals)
    Nothing -> Left NoCategoriesOrValues

build_ :: forall c n a.
          ( Eq a
          , KnownNat c
          , KnownNat n
          )
       => Maybe (S.Vector c a)
       -> S.Vector n a
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
