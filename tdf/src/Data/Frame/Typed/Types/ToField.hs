{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Frame.Typed.Types.ToField
  ( ToField(..)
  ) where

import           Data.Frame.Prelude   hiding ( empty
                                             , head
                                             , map
                                             , toList
                                             )

import           Data.String                 ( String )
import qualified Data.Text           as Text

class ToField a where
  toField :: a -> Text

instance ToField a => ToField (Maybe a) where
  toField = maybe mempty toField

instance ToField Bool where
  toField = show

instance ToField Text where
  toField = identity

instance ToField Integer where
  toField = Text.pack . show

instance ToField Int where
  toField = Text.pack . show

instance ToField String where
  toField = Text.pack

-- TODO uhhhh
instance ToField Float where
  toField = Text.pack . show
