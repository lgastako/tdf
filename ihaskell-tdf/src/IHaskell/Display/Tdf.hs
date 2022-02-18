{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module IHaskell.Display.Tdf
  where

import           TDF.Prelude

import           Data.String                      ( fromString )
import           IHaskell.IPython.Types           ( MimeType( MimeHtml ) )
import           IHaskell.Display                 ( Display( Display )
                                                  , DisplayData( DisplayData )
                                                  , IHaskellDisplay( display )
                                                  )
import qualified Lucid
import           Lucid                            ( Html )
import           Lucid.Html5                      ( table_
                                                  , tbody_
                                                  , td_
                                                  , th_
                                                  , thead_
                                                  , tr_
                                                  )
import           TDF.Frame                    ( Frame )
import qualified TDF.Frame          as DF
import           TDF.Series                       ( Series )
import qualified TDF.Series             as Series
import           TDF.Types.ToField                ( ToField )

instance ( AllUniqueLabels (Map (Vec n) a)
         , Forall a Typeable
         , Forall a Unconstrained1
         , Forall a ToField
         , Forall (Map (Vec n) a) Unconstrained1
         , SNatI n
         ) => IHaskellDisplay (Frame n Int a) where
  display df = do
    let (header:rows) =  DF.toTexts df
        --    ^-- should always have at least a header if we got here

    pure $ Display
      [ DisplayData MimeHtml . cs . Lucid.renderText
        $ template header rows
      ]

instance ( Show a
         , SNatI n
         )
      => IHaskellDisplay (Series n Int a) where
  display series = do
    let (header:rows) =  Series.toTexts series
        --    ^-- should always have at least a header if we got here

    pure $ Display
      [ DisplayData MimeHtml . cs . Lucid.renderText
        $ template header rows
      ]

template :: [Text] -> [[Text]] -> Html ()
template header rows = table_ $ do
  thead_ . tr_ . traverse_ headerCell $ header
  tbody_ . traverse_ row $ rows
  where
    headerCell = th_ . fromString . cs
    rowCell    = td_ . fromString . cs
    row        = tr_ . traverse_ rowCell
