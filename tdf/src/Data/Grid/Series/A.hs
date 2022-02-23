{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Grid.Series.A
  ( ASeries(..)
  -- Constructors
  , a
  , empty
  -- Optics
  , at
--  , series
  -- Helpers
  , reify
  ) where

import Data.Grid.Prelude hiding ( empty )
import Data.Grid.Series         ( Series )

import qualified Data.Grid.Series  as S

data ASeries k a = forall n. KnownNat n
  => ASeries { size :: Int
             , unASeries :: Series n k a
             }

-- ================================================================ --
--   Constructors
-- ================================================================ --

a :: forall n k a. KnownNat n => Series n k a -> ASeries k a
a s = ASeries (fromIntegral . natVal $ Proxy @n) s

-- ================================================================ --
--   Optics
-- ================================================================ --

_series :: forall n k a.
           KnownNat n
        => Lens' (ASeries k a)
                 (Series n k a)
_series = panic "Series.A.series"

-- lens g s
  -- where
  --   g :: forall n.
  --        ASeries k a
  --     -> Series n k a
  --   g (ASeries s) =
  --     let _ = s :: Series n k a
  --     in s
  --     where
  --       n :: Int
  --       n = fromIntegral (natVal @n Proxy)
  --       -- n = natVal @n Proxy

  --   s :: ASeries k a -> Series n k a -> ASeries k a
  --   s = undefined

-- ================================================================ --
--   Helpers
-- ================================================================ --

empty :: forall k a.
         Enum k
      => ASeries k a
empty = a S.empty

-- fromList :: forall  k a.
--             ( Enum k )
--          => [a]
--          -> ASeries k a
-- fromList xs = a_ g
--   where
--     f :: Maybe (Series n k a) -> Series n k a
--     f = (`onCrash` "foo")

--     g :: forall n. KnownNat n => Series n k a -> (
--     g = undefined
--     -- (const $ S.fromList xs)

at :: forall k a.
      ( Enum k
--      , KnownNat n
      )
   => k
   -> Lens' (ASeries k a) (Maybe a)
at _k = panic "Grid.Series.A.at"

-- lens g s
--   where
--     g :: ASeries k a -> Maybe a
--     g s' = undefined --  (s' ^.) . iat <$> I.position k (s' ^. index)

--     s :: forall n.
--          KnownNat n
--       => ASeries k a
--       -> Maybe a
--       -> Series n k a
--     s s' = undefined
--     -- = \case
--     --   Nothing -> s'
--     --   Just v -> case I.position k (s' ^. index) of
--     --     Nothing -> s'
--     --     Just n  -> set (iat n) v s'

-- ================================================================ --
--   Eliminators
-- ================================================================ --

reify :: forall k a r.
         (forall n. KnownNat n => Series n k a -> r)
      -> ASeries k a
      -> r
reify f (ASeries _n s) = f s

-- ================================================================ --
--   Helpers
-- ================================================================ --

_s1 :: Series 3 Int Bool
_s1 = pure True

_s2 :: Series 2 Int Bool
_s2 = pure False

_as1 :: ASeries Int Bool
_as1 = a _s1

_as2 :: ASeries Int Bool
_as2 = a _s2
