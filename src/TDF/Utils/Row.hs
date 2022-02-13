{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

module TDF.Utils.Row
  ( fieldLabels
  ) where

import           TDF.Prelude
import           TDF.Types.ToField
import qualified Data.Row.Records as Rec

-- TODO move to Data.Row.X or similar

fieldLabels :: Forall a ToField => Rec a -> [Text]
fieldLabels = Rec.erase @ToField @_ @Text toField
