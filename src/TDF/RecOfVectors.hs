module TDF.RecOfVectors
  ( revectorize
  ) where

import Data.Vector ( Vector )
import Data.Row    ( Rec )

-- TODO
revectorize :: Vector (Rec a) -> Maybe (Rec b)
revectorize _recs = error "unpossible!"

