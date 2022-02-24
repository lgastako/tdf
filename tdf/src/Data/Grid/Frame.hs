{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid.Frame
  ( Frame
  -- Constructors
  , fromSeries
  -- Combinators
  , resetColumNamesFromIndexes
  , transpose
  -- Optics
  , col
  , colLabels
  , colSeries
  , row
  , rowLabels
  -- Eliminators
  , display
  , toTexts
  ) where

import Data.Grid.Prelude hiding ( transpose )

import Data.Frame.Typed.Table ( Table )
import Data.Grid.Index        ( Index )
import Data.Grid.Name         ( Name )
import Data.Grid.Series       ( Series )
import Data.Renderable        ( Renderable( render ) )

import qualified Data.Frame.Typed.Table as Table
import qualified Data.Grid.Index        as I
import qualified Data.Grid.Name         as Name
import qualified Data.Grid.Series       as S
import qualified Data.List              as L
import qualified Data.Vector.Sized      as Sized

newtype Frame (c :: Nat) (r :: Nat) ci ri a
  = Frame (Series c ci (Series r ri a))
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance ( Universal a
         , Universal ri
         , Universal ci
         ) => Universal (Frame c r ci ri a)

instance ( Enum ci
         , Enum ri
         , KnownNat c
         , KnownNat r
         , Renderable a
         ) => Renderable (Frame c r ci ri a) where
  render = Table.render . toTable

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromSeries :: forall c r ci ri a.
              Series c ci (Series r ri a)
           -> Frame (c :: Nat) (r :: Nat) ci ri a
fromSeries = Frame

-- ================================================================ --
--   Optics
-- ================================================================ --

col :: forall c r ci ri a.
       Finite c
    -> Lens' (Frame c r ci ri a)
             (Series r ri a)
col c = colSeries . S.vector . Sized.ix c

colLabels :: forall c r ci ri a.
             ( Enum ci
             , KnownNat c
             )
          => Lens' (Frame c r ci ri a)
                   (Sized.Vector c ci)
colLabels = colSeries . S.index . I.vector

rowLabels :: forall c r ci ri a.
             Lens' (Frame c r ci ri a)
                   (Sized.Vector r ri)
rowLabels = panic "rowLabels"

colSeries :: Iso' (Frame c r ci ri a)
                  (Series c ci (Series r ri a))
colSeries = coerced

row :: forall c r ci ri a.
       ( Enum ci
       , Enum ri
       , KnownNat c
       , KnownNat r
       )
    => Finite r
    -> Lens' (Frame c r ci ri a)
             (Series c ci a)
row r = rowSeries . S.vector . Sized.ix r

rowSeries :: ( Enum ci
             , Enum ri
             , KnownNat c
             , KnownNat r
             )
          => Lens' (Frame c r ci ri a)
                   (Series r ri (Series c ci a))
rowSeries = colSeries . transposed

transposed :: ( Enum ci
              , Enum ri
              , KnownNat c
              , KnownNat r
              )
           => Iso' (Series c ci (Series r ri a))
                   (Series r ri (Series c ci a))
transposed = iso get' set'
  where
    get' = transposeSeries
    set' = transposeSeries

-- ================================================================ --
--   Combinators
-- ================================================================ --

resetColumNamesFromIndexes  :: forall c r ci ri a.
                               ( Enum ci
                               , KnownNat c
                               , Renderable ci
                               )
                            => Frame c r ci ri a
                            -> Frame c r ci ri a
resetColumNamesFromIndexes = colSeries %~ f
  where
    f :: Series c ci (Series r ri a)
      -> Series c ci (Series r ri a)
    f sc = S.op g idx sc
      where
        _ = S.op :: (Index c ci -> Series r ri a -> Series r ri a)
                 -> Index c ci
                 -> Series c ci (Series r ri a)
                 -> Series c ci (Series r ri a)

        idx :: Index c ci
        idx = sc ^. S.index

    g :: ci -> Series r ri a -> Series r ri a
    g ci s = s & S.name .~ ciToName ci
      where
        ciToName :: ci -> Name
        ciToName = Name.unsafeFromText . render

transpose :: forall c r ci ri a.
             ( Enum ci
             , Enum ri
             , KnownNat c
             , KnownNat r
             )
          => Frame c r ci ri a
          -> Frame r c ri ci a
transpose = Frame . transposeSeries . view colSeries

transposeSeries :: forall c r ci ri a.
                   ( Enum ci
                   , Enum ri
                   , KnownNat c
                   , KnownNat r
                   )
                => Series c ci (Series r ri a)
                -> Series r ri (Series c ci a)
transposeSeries = S.fromVector
  . map S.fromVector
  . orCrash "error2"
  . Sized.fromList
  . orCrash "error1"
  . traverse Sized.fromList
  . L.transpose
  . map Sized.toList
  . Sized.toList
  . map (view S.vector)
  . view S.vector

-- ================================================================ --
--   Eliminators
-- ================================================================ --

display :: forall c r ci ri a.
           ( Enum ci
           , Enum ri
           , KnownNat c
           , KnownNat r
           , Renderable a
           )
        => Frame c r ci ri a
        -> IO ()
display = putStr . Table.render . toTable

toTable :: forall c r ci ri a.
           ( Enum ci
           , Enum ri
           , KnownNat c
           , KnownNat r
           , Renderable a
           )
        => Frame c r ci ri a
        -> Table
toTable = orCrash "display explode"
  . Table.fromHeadedRows
  . L.map Table.Row
  . toTexts

toTexts :: forall c r ci ri a.
           ( Enum ci
           , Enum ri
           , KnownNat c
           , KnownNat r
           , Renderable a
           )
        => Frame c r ci ri a
        -> [[Text]]
toTexts f = ts
  where
    ts :: [[Text]]
    ts = Sized.toList vs

    vs :: Sized.Vector r [Text]
    vs = ss'' ^. S.vector

    ss'' :: Series r ri [Text]
    ss'' = map (uncurry (:)) ss'

    ss' :: Series r ri (Text, [Text])
    ss' = map S.toTexts ss

    ss :: Series r ri (Series c ci a)
    ss = f' ^. colSeries

    f' :: Frame r c ri ci a
    f' = transpose f
