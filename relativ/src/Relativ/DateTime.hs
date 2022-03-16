{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Relativ.DateTime
  ( Ambiguity(..)
  , Config(..)
  , DateTimeIndex
  , Normalize(..)
  , build
  , buildDef
  , def
  ) where

import Relativ.Prelude

import Relativ.Types.DateTime ( DateTime )
import Relativ.Types.Flag     ( Flag
                              , fromFlag
                              , toFlag
                              )
import Relativ.Types.Name     ( Name )

import qualified Data.Vector.Sized as S

data Ambiguity (n :: Nat)
  = Infer
  | BitVec (S.Vector n Bool)
  | Raise
  deriving (Eq, Ord, Show)

-- | Index of `datetime64` data.
data DateTimeIndex (n :: Nat) = DateTimeIndex
  { ambiguity :: Ambiguity n
  , name      :: Maybe Name
  , values    :: S.Vector n DateTime
  } deriving (Eq, Ord, Show)

data Normalize = Normalize
  deriving (Eq, Ord, Show)

data Config (n :: Nat) = Config
  { ambiguity :: Ambiguity n
  , name      :: Maybe Name
  , normalize :: Flag Normalize
  } deriving (Eq, Ord, Show)

def :: Config n
def = Config
  { ambiguity = Infer
  , name      = Nothing
  , normalize = toFlag Normalize False
  }

build :: forall n.
         Config n
      -> S.Vector n DateTime
      -> DateTimeIndex n
build Config {..} v = DateTimeIndex
  { ambiguity = ambiguity
  , name      = name
  , values    = maybeNormalize normalize v
  }

buildDef :: forall n.
         S.Vector n DateTime
      -> DateTimeIndex n
buildDef = build def

maybeNormalize :: forall n.
                  Flag Normalize
               -> S.Vector n DateTime
               -> S.Vector n DateTime
maybeNormalize n vs
  | fromFlag Normalize n = normalizeToMidnight vs
  | otherwise            = vs

normalizeToMidnight :: forall n.
                       S.Vector n DateTime
                    -> S.Vector n DateTime
normalizeToMidnight = panic "normalizeToMidnight"
