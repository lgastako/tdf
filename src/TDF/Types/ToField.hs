{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}

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

instance ToField Text where
  toField = identity

instance ToField Integer where
  toField = Text.pack . show

instance ToField Int where
  toField = Text.pack . show

instance ToField String where
  toField = Text.pack
