{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Renderable
  ( Renderable(..)
  ) where

import Data.Grid.Prelude

import qualified Data.Text as T

class Renderable r where
  render :: r -> Text

instance Renderable Text where
  render = identity

instance Renderable () where
  render = show

instance Renderable Int where
  render = show

instance Renderable Integer where
  render = show

instance Renderable Float where
  render = show

instance Renderable Double  where
  render = show

instance Renderable Bool where
  render = show

instance Renderable Char where
  render = show

instance Renderable a => Renderable (Ratio a) where
  render x = render (numerator x) <> " % " <> render (denominator x)

instance Renderable a => Renderable (Maybe a) where
  render x = "Maybe " <> render x

instance (Renderable e, Renderable a) => Renderable (Either e a) where
  render = \case
    Left  e -> "Left "  <> render e
    Right a -> "Right " <> render a

instance (Renderable a, Renderable b) => Renderable (a, b) where
  render (a, b) = "(" <> render a <> "," <> render b <> ")"

instance Renderable a => Renderable [a] where
  render = ("[" <>) . (<> "]") . T.intercalate "," . map render
