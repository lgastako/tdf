{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Types.ToField
  ( ToField(..)
  ) where

import           TDF.Prelude  hiding ( empty
                                     , head
                                     , map
                                     , toList
                                     )

import           Data.String         ( String )
import qualified Data.Text   as Text

class ToField a where
  toField :: a -> Text

instance ToField a => ToField (Maybe a) where
  toField = \case
    Nothing -> mempty
    Just x  -> toField x

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