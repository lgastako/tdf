{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

module Data.Random.X
  ( randoms
  ) where

import Data.Frame.Prelude

import System.Random.Mersenne ( MTGen
                              , newMTGen
                              )
import System.IO.Unsafe       ( unsafePerformIO )

import qualified System.Random.Mersenne as G

{-# NOINLINE theOneTrueGen #-}
theOneTrueGen :: MTGen
theOneTrueGen = unsafePerformIO $ newMTGen useTimeAsSeed
  where
    useTimeAsSeed = Nothing

randoms :: (G.MTRandom a, Num a) => IO [a]
randoms = G.randoms theOneTrueGen

_demo :: Int -> IO ()
_demo n = traverse_ print =<< take n <$> randoms @Double
