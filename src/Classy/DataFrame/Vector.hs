{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Classy.DataFrame.Vector where

import Classy.DataFrame
import Data.Vector

-- class Frameable a where
--   data Label a :: *
--   data Key a   :: *

--   label   :: a -> Label a
--   valueAt :: Key a -> a -> b

-- class Frameable a => DataFrame a where
--   headN :: Int -> a -> a
--   headN = undefined

--   tailN :: Int -> a -> a
--   tailN = undefined

-- --  column ::

--   {-# MINIMAL headN, tailN #-}

--   head :: a -> a
--   head = headN 1

--   tail :: a -> a
--   tail = tailN 1

-- instance Frameable a => DataFrame (Vector a) where
