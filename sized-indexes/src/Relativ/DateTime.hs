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

data Ambiguity (n :: Nat)
  = Infer
  | BitVec (Vec n Bool)
  | Raise
  deriving (Eq, Ord, Show)

-- | Index of `datetime64` data.
data DateTimeIndex (n :: Nat) = DateTimeIndex
  { ambiguity :: Ambiguity n
  , name      :: Maybe Name
  , values    :: Vec n DateTime
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
      -> Vec n DateTime
      -> DateTimeIndex n
build Config {..} v = DateTimeIndex
  { ambiguity = ambiguity
  , name      = name
  , values    = maybeNormalize normalize v
  }

buildDef :: forall n.
         Vec n DateTime
      -> DateTimeIndex n
buildDef = build def

maybeNormalize :: forall n.
                  Flag Normalize
               -> Vec n DateTime
               -> Vec n DateTime
maybeNormalize n vs
  | fromFlag Normalize n = normalizeToMidnight vs
  | otherwise            = vs

normalizeToMidnight :: forall n.
                       Vec n DateTime
                    -> Vec n DateTime
normalizeToMidnight = panic "normalizeToMidnight"
