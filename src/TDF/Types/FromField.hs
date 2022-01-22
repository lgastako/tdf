{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Types.FromField
  ( FromField(..)
  , genericReadFF
  ) where

import TDF.Prelude  hiding ( empty
                           , head
                           , map
                           , toList
                           )

import Data.String         ( String )

class FromField a where
  fromField :: Text -> Either String a

instance {-# OVERLAPPABLE #-} Read a => FromField a where
  fromField = genericReadFF "Readable"

instance FromField Text where
  fromField = Right

instance FromField LText where
  fromField = Right . cs

instance FromField ByteString where
  fromField = Right . cs

instance FromField LByteString where
  fromField = Right . cs

instance FromField String where
  fromField = Right . cs

genericReadFF :: ( Read b
                 , StringConv a String
                 , Show a
                 )
              => [Char]
              -> a
              -> Either [Char] b
genericReadFF s t = cs t
    & readMaybe
    & maybe (Left $ "Invalid " <> s <> ": " ++ show t) Right
