{-# LANGUAGE NoImplicitPrelude #-}

module Period.TestYear
  ( prop_Year_roundtrip
  ) where

import Relativ.Prelude

import qualified Test.QuickCheck as QC

import Orphans ()
import Relativ.Types.Period.Year ( Year )

import qualified Relativ.Types.Period.Year as Year

_qc :: QC.Testable prop => prop -> IO ()
_qc = QC.quickCheck

prop_Year_roundtrip :: Year -> Bool
prop_Year_roundtrip y = forwards y && backwards (Year.toInt y)
  where
    forwards  n = (n == ) . Year.fromInt . Year.toInt $ n
    backwards n = (n == ) . Year.toInt . Year.fromInt $ n
