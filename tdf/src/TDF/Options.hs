{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Options
  ( Options( optData
           , optIndex
           )
  , empty
  , fromVec
  ) where

import           TDF.Prelude              hiding ( empty )

import qualified Data.Vec.Lazy        as Vec
import           TDF.Index                       ( Index )
import qualified TDF.Index            as Index

data Options (n :: Nat) idx a = Options
  { optIndex :: Index n idx
  , optData  :: Vec n (Rec a)
  }

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromVec :: SNatI n
        => Vec n (Rec a)
        -> Options n Int a
fromVec v = Options
  { optIndex = case Index.defaultIntsFor v of
      Just idx -> idx
      Nothing  -> panic "Options.fromVec.1"
  , optData  = v
  }

empty :: Options 'Z Int Empty
empty = Options
  { optIndex = case Index.defaultIntsFor v of
      Just idx -> idx
      Nothing  -> panic "Options.empty.1"
  , optData  = v
  }
  where
    v = Vec.empty

-- ================================================================ --
--  Combinators
-- ================================================================ --

-- ================================================================ --
--   Eliminators
-- ================================================================ --
