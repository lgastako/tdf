{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Relativ.Types.Range
  ( Range
  , defaultStep
  -- , fromThen
  -- , fromThenTo
  , fromTo
  -- , toList
  ) where

import Relativ.Prelude hiding ( from
                              , toList
                              )

data Range (n :: Nat) a = Range
  { start :: a
  , stop  :: a
  , step  :: Int
  } deriving (Eq, Ord, Show)

defaultStep :: Int
defaultStep = 1

-- ================================================================ --
--   Constructors
-- ================================================================ --

fromTo :: forall n a.
          ( Eq a
          , Integral a
          , Num a
          , Ord a
          , KnownNat n
          )
       => a
       -> a
       -> Maybe (Range n a)
fromTo = panic "Range.fromTo"
-- fromTo x y = case testEquality nSNat rangeSize of
--   Nothing -> Nothing
--   Just Refl -> Just $ Range
--     { start = x
--     , stop  = y
--     , step  = defaultStep
--     }
--   where
--     --nSNat :: Natural
--     nSNat = natVal (Proxy :: Proxy n)

--     rangeSize :: forall q. SNat q
--     rangeSize = undefined
--       where
--         q :: SNatI q => SNat q
--         q = snat @q

-- -- reify :: forall r. Nat -> (forall n. SNatI n => Proxy n -> r) -> r
--         jj :: SNat q
--         jj = undefined

--         -- jj = TNat.reify deltaNat f
--         qq :: SNatI q => SNat n
--         qq = case testEquality jj q of
--           Nothing -> undefined


--         f :: (forall gg. SNatI gg => Proxy gg -> SNat gg)
--         f (Proxy :: Proxy n) = snat @n

    -- rangeSize = TNat.reify deltaNat
    --   --(undefined :: Proxy w -> SNat q)
    --   (\(Proxy :: Proxy p) -> snat @p)

    -- deltaNat :: Natural
    -- deltaNat = Nat.fromNatural delta

    -- delta :: Natural
    -- delta = fromIntegral $ y - x

    -- rangeSize
    --   | signum (y - x) > 0 = panic "fromTo.rangeSize.1"
    --   | otherwise = case testEquality nSNat SZ of
    --       Just Refl -> SZ
    --       Nothing   -> panic "Was 0 when not expected"

-- fromThen :: a -> a -> Range a
-- fromThen x y = Range
--   { start = x
--   , rangeThen = Just y
--   , stop   = Nothing
--   }

-- fromThenTo :: a -> a -> a -> Range a
-- fromThenTo x y z = Range
--   { start = x
--   , rangeThen = Just y
--   , stop   = Just z
--   }

-- toList :: Enum a => Range a -> [a]
-- toList Range {..} = case stop of
--   Nothing -> case rangeThen of
--     Nothing -> enumFrom start
--     Just rThen -> enumFromThen start rThen
--   Just rTo -> case rangeThen of
--     Nothing -> enumFromTo start rTo
--     Just rThen -> enumFromThenTo start rThen rTo
