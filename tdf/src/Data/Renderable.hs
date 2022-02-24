{-# LANGUAGE NoImplicitPrelude #-}

module Data.Renderable
  ( Renderable(..)
  ) where

import Data.Grid.Prelude

class Renderable r where
  render :: r -> Text

instance Renderable Text where
  render = identity

instance Renderable () where
  render = show

instance Renderable Int where
  render = show

instance Renderable Bool where
  render = show
