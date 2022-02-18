{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Frame.Typed.SubIndex
  ( SubIndex(..)
  -- Eliminators
  , index
  ) where

import           Data.Frame.Prelude
import qualified Data.List          as List
import qualified Data.Vec.Lazy.X    as Vec

class SNatI n => SubIndex f (n :: Nat) idx where
  drop :: f n idx -> f m idx
  take :: f n idx -> f m idx

  toLst :: f n idx -> [idx]
  toLst = Vec.toList . toVec

  toVec :: f n idx -> Vec n idx
  default toVec :: SNatI n => f n idx -> Vec n idx
  toVec = orCrash "SubIndex.toVec: wrong size"
    . Vec.fromList
    . List.take n
    . toLst
    where
      n = fromIntegral . snatToNat $ snat @n

  {-# MINIMAL (toLst | toVec)
            , drop
            , take
    #-}

-- ================================================================ --
--   Eliminators
-- ================================================================ --

-- TODO: I think this has the problem that it will try break if there
--       are more indexes than items, and in fact it should only break
--       if there are more items than indexes.
-- TODO: Write appropriate tests.
index :: forall n idx a sub.
         ( SNatI n
         , SubIndex sub n idx
         )
      => sub n idx
      -> Vec n a
      -> Vec n (idx, a)
index subIdx xs = Vec.zip (toVec subIdx) xs
