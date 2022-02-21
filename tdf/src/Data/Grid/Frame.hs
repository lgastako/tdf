{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid.Frame
  ( Frame
  -- Combinators
  , transpose
  ) where

import Data.Grid.Prelude hiding ( transpose )

import Data.Grid                ( Series )

newtype Frame (c :: Nat) (r :: Nat) ci ri a
  = Frame (Series c ci (Series r ri a))
  deriving (Eq, Ord, Show)

instance ( Universal a
         , Universal ri
         , Universal ci
         ) => Universal (Frame r c ri ci a)

-- ================================================================ --
--   Optics
-- ================================================================ --

_colSeries :: Lens' (Frame c r ci ri a)
                    (Series c ci (Series r ri a))
_colSeries = coerced

-- ================================================================ --
--   Combinators
-- ================================================================ --

transpose :: forall c r ci ri a.
             Frame c r ci ri a
          -> Frame r c ri ci a
transpose _f = panic "transpose"
