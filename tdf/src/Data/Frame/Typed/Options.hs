{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Frame.Typed.Options
  ( Options( optData
           , optIndex
           )
  , empty
  , fromVec
  ) where

import           Data.Frame.Prelude       hiding ( empty )

import qualified Data.Vec.Lazy          as Vec
import           Data.Frame.Typed.Index          ( Index )
import qualified Data.Frame.Typed.Index as Index

data Options (n :: Nat) idx a = Options
  { optIndex :: Index n idx
  , optData  :: Vec n (Rec a)
  }

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromVec :: forall n idx a.
           ( Enum idx
           , SNatI n
           )
        => Vec n (Rec a)
        -> Options n idx a
fromVec = Options <$> Index.defaultFor <*> identity

empty :: Options 'Z Int Empty
empty = fromVec Vec.empty

-- ================================================================ --
--  Combinators
-- ================================================================ --

-- ================================================================ --
--   Eliminators
-- ================================================================ --
