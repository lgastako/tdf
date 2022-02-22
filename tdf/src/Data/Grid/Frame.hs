{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid.Frame
  ( Frame
  -- Constructors
  , fromSeries
  -- Combinators
  , transpose
  -- Optics
  , colSeries
  -- Eliminators
  , display
  , toTexts
  ) where

import Data.Grid.Prelude hiding ( transpose )

import Data.Frame.Typed.Table ( Table )
import Data.Grid              ( Series )
import Data.Grid.Renderable   ( Renderable( render ) )

import qualified Data.Frame.Typed.Table as Table
import qualified Data.Grid.Series       as S
import qualified Data.List              as L
import qualified Data.Vector.Sized      as Sized

newtype Frame (c :: Nat) (r :: Nat) ci ri a
  = Frame (Series c ci (Series r ri a))
  deriving (Eq, Ord, Show)

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

colSeries :: Iso' (Frame c r ci ri a)
                  (Series c ci (Series r ri a))
colSeries = coerced

-- ================================================================ --
--   Combinators
-- ================================================================ --

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
