{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
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

-- instance {-# OVERLAPPABLE #-} Read a => FromField a where
--   fromField = genericReadFF "Readable"

instance FromField Int where
  fromField = genericReadFF "Int"

instance (FromField a, Read a) => FromField (Maybe a) where
  fromField s = (Just <$> fromField s) <|> pure Nothing

instance FromField Bool where
  fromField = genericReadFF "Bool"

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

instance FromField Float where
  fromField = genericReadFF "Float"

instance FromField Double where
  fromField = genericReadFF "Double"

genericReadFF :: ( Read b
                 , StringConv a String
                 , Show a
                 )
              => String
              -> a
              -> Either String b
genericReadFF s t = cs t
  & readMaybe
  & maybe (Left $ "Invalid " <> s <> ": " ++ show t) Right
