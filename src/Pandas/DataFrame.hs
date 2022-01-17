module Pandas.DataFrame
  (
  ) where

import Data.Vector

newtype DataFrame a = DataFrame (Vector a)


