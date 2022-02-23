{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TensorTest
  ( prop_mapRoundTrip
  ) where

import Protolude

import Orphans ()

import qualified Data.Tensor as F

prop_mapRoundTrip :: Map Bool Bool -> Bool
prop_mapRoundTrip m = (m & F.toMap   . F.fromMap) == m
                   && (m'& F.fromMap . F.toMap  ) == m'
  where
    m' = F.fromMap m
