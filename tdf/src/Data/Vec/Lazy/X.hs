module Data.Vec.Lazy.X
  ( module Data.Vec.Lazy
  -- Constructors
  , dwimFromList
  , unsafeFromList
  -- Combinators
  , zip
  ) where

import           Prelude        hiding ( filter
                                       , foldr
                                       , zip
                                       , zipWith
                                       )

import qualified Data.List     as List
import           Data.Maybe            ( fromMaybe )
import           Data.Type.Nat         ( SNatI
                                       , snat
                                       , snatToNat
                                       )
import           Data.Vec.Lazy

-- ================================================================ --
--   Constructors
-- ================================================================ --

dwimFromList :: forall n a. SNatI n => [a] -> Vec n a
dwimFromList = unsafeFromList . List.take m . List.cycle
  where
    m = fromIntegral . snatToNat $ snat @n

unsafeFromList :: forall n a. SNatI n => [a] -> Vec n a
unsafeFromList = fromMaybe (error msg) . fromList
  where
    msg = "unsafeFromList called on list of wrong size."

-- ================================================================ --
--   Combinators
-- ================================================================ --

zip :: Vec n a -> Vec n b -> Vec n (a, b)
zip = zipWith (,)
