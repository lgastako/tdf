-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
-- {-# LANGUAGE RankNTypes            #-}

module Data.Grid.Index.Class
  ( -- AnyIndex(..)
  ) where

-- import Data.Grid.Prelude

-- import qualified Data.Vector.Sized as Sized

-- class AnyIndex a n k where
--   at       :: Proxy n -> k -> Lens' a (Maybe k)
--   iat      :: Finite n -> a -> k
--   position :: k -> a -> Maybe (Finite n)
--   toVector :: a -> Sized.Vector n k
