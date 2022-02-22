{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Grid.Series
  ( Series
  -- Constructors
  , empty
  , fromList
  , fromListWith
  , fromVector
  , fromVectorWith
  , single
  , singleWith
  -- Optics
  , at
  , iat
  , index
  , name
  , vector
  -- Combinators
  , (++)
  -- Eliminators
  , display
  , thaw
  , toTexts
  ) where

import Data.Grid.Prelude hiding ( (++)
                                , empty
                                )

import Data.Grid.Index          ( Index )
import Data.Grid.Name           ( Name )
import Data.Grid.Renderable     ( Renderable
                                , render
                                )

import qualified Data.Frame.Typed.Table   as Table
import qualified Data.Grid.Index          as I
import qualified Data.Grid.Name           as Name
import qualified Data.Grid.Series.Mutable as M
import qualified Data.List                as L
import qualified Data.Vector.Sized.X      as Sized

data Series (n :: Nat) k a = Series
  { _name   :: Name
  , _index  :: Index n k
  , _vector :: Sized.Vector n a
  } deriving (Eq, Functor, Foldable, Generic, Ord, Show, Traversable)

instance ( Universal a
         , Universal idx
         ) => Universal (Series n idx a)

instance (Enum k, KnownNat n) => Applicative (Series n k) where
  pure x = Series
    { _name   = seriesName
    , _index  = I.default_
    , _vector = Sized.replicate x
    }

  (<*>) :: forall a b.
           Series n k (a -> b)
        -> Series n k a
        -> Series n k b
  f <*> x = Series
    { _name   = panic "Grid.Series.(<*>)._name"
    , _index  = I.apForSeries     (f ^. index)  (x ^. index)  -- TODO uhhh....
    , _vector = Sized.zipWith ($) (f ^. vector) (x ^. vector)
    }

instance (Enum k, KnownNat n) => Monad (Series n k) where
  (>>=) :: forall a b.
           Series n k a
        -> (a -> Series n k b)
        -> Series n k b
  ma >>= mf = join' (map mf ma)

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: Enum k => Series 0 k a
empty = Series
  { _name   = seriesName
  , _index  = I.empty
  , _vector = Sized.empty
  }

fromList :: forall n k a.
                ( Enum k
                , KnownNat n
                )
             => [a]
             -> Maybe (Series n k a)
fromList = fromListWith I.default_

fromListWith :: forall n k a.
                ( KnownNat n )
             => Index n k
             -> [a]
             -> Maybe (Series n k a)
fromListWith idx = map mkSeries . Sized.fromList
  where
    mkSeries v = Series
      { _name   = Name.unsafeFromText "series"
      , _index  = idx
      , _vector = v
      }

fromVector :: forall n k a.
              ( Enum k
              , KnownNat n
              )
           => Sized.Vector n a
           -> Series n k a
fromVector = fromVectorWith I.default_

fromVectorWith :: forall n k a.
                  ( KnownNat n )
               => Index n k
               -> Sized.Vector n a
               -> Series n k a
fromVectorWith idx v = Series
  { _name   = Name.unsafeFromText "series"
  , _index  = idx
  , _vector = v
  }

single :: forall a k.
          ( Enum k )
       => a
       -> Series 1 k a
single = singleWith I.default_

singleWith :: forall k a.
              Index 1 k
           -> a
           -> Series 1 k a
singleWith idx x = Series
  { _name   = seriesName
  , _index  = idx
  , _vector = Sized.singleton x
  }

-- ================================================================ --
--   Optics
-- ================================================================ --

-- TODO: Sprinkle
-- https://hackage.haskell.org/package/lens-4.16/docs/Data-Vector-Lens.html#v:forced
-- in appropriately

at :: forall n k a.
      ( Enum k
      , KnownNat n
      )
   => k
   -> Lens' (Series n k a) (Maybe a)
at k = lens g s
  where
    g :: Series n k a -> Maybe a
    g s' = (s' ^.) . iat <$> I.position k (s' ^. index)

    s :: Series n k a -> Maybe a -> Series n k a
    s s' = \case
      Nothing -> s'
      Just v -> case I.position k (s' ^. index) of
        Nothing -> s'
        Just n  -> set (iat n) v s'

iat :: forall n k a.
       Finite n
    -> Lens' (Series n k a) a
iat _n = lens g s
  where
    g :: Series n k a -> a
    g = view (vector . something)
      where
        something :: Getter (Sized.Vector n a) a
        something = panic "Grid.Series.something"

    s ::  Series n k a -> a ->  Series n k a
    s = panic "Grid.Series.iat.g"

index :: forall n k a.
         Lens' (Series n k a)
               (Index n k)
index = field @"_index"

name :: forall  n k a.
        Lens' (Series n k a)
              Name
name = field @"_name"

vector :: forall n k a.
          Lens' (Series n k a)
                (Sized.Vector n a)
vector = field @"_vector"

-- ================================================================ --
--   Combinators
-- ================================================================ --

(++) :: forall m n k a.
        ( Enum k
        , KnownNat m
        , KnownNat n
        , Ord k
        )
     => Series m k a
     -> Series n k a
     -> Series (m + n) k a
a ++ b = Series
  { _name   = seriesName
  , _index  = ix'
  , _vector = v'
  }
  where
    ix' = (a ^. index)  I.++     (b ^. index)
    v'  = (a ^. vector) Sized.++ (b ^. vector)

-- ================================================================ --
--   Eliminators
-- ================================================================ --

display :: forall n k a. (Show k, Renderable a) => Series n k a -> IO ()
display = putStr
  . Table.render
  . (`onCrash` "Series.display")
  . Table.fromHeadedRows
  . L.map (Table.Row . pure)
  . uncurry (:)
  . toTexts

thaw :: forall n k mm s a.
        ( Applicative mm
        , KnownNat n
        , PrimMonad mm
        , s ~ PrimState mm
        )
     => Series n k a
     -> mm (M.MSeries n k s a)
thaw s = M.fromVectorWith (s ^. index) =<< Sized.thaw (s ^. vector)

toTexts :: Renderable a
        => Series n k a
        -> (Text, [Text])
toTexts = (,) <$> view (name . to Name.unName) <*>  map render . toList

-- ================================================================ --
--   Helpers & Misc Junks
-- ================================================================ --

join' :: forall n idx a.
         ( Enum idx
         )
      => Series n idx (Series n idx a)
      -> Series n idx a
join' _s = panic "Grid.Series.join'" -- s & vector %~ f
  -- where
  --   f :: Vector n a -> Vector n a
  --   f = undefined -- panic "join'" -- updateVec (Sized.join . fmap toVec)

  --   unpack :: ()
  --   unpack = undefined

seriesName :: Name
seriesName = Name.unsafeFromText "series"

-- Temp example
_s :: Series 3 Int Float
_s = fromList [1..3] `onCrash` "Gird.series.s.boom"
