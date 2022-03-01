{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module IHaskell.Display.Tdf
  ( chartExample
  , template
  ) where

import Data.Frame.Prelude

import Data.Rect                ( Rect )
import Data.Frame.Typed           ( Frame )
import Data.Frame.Typed.Series    ( Series )
import Data.Frame.Typed.ToField   ( ToField )
import Data.Renderable            ( Renderable )
import Data.String                ( fromString )
import IHaskell.IPython.Types     ( MimeType( MimeHtml ) )
import IHaskell.Display           ( Display( Display )
                                  , DisplayData( DisplayData )
                                  , IHaskellDisplay( display )
                                  )
import IHaskell.Display.Tdf.Chart ( chartExample )
import Lucid                      ( Html )
import Lucid.Html5                ( table_
                                  , tbody_
                                  , td_
                                  , th_
                                  , thead_
                                  , tr_
                                  )

import qualified Lucid
import qualified Data.Frame.Typed        as DF
import qualified Data.Frame.Typed.Series as Series
import qualified Data.Grid               as G
import qualified Data.Rect             as SQ

instance Show a => IHaskellDisplay (G.Series n ix a) where
  display df = do
    let (header:rows) = G.seriesToTexts df
    pure $ Display
      [ DisplayData MimeHtml . cs . Lucid.renderText
        $ template header rows
      ]

instance Renderable a => IHaskellDisplay (G.Frame r ri c ci a) where
  display df = do
    let (header:rows) = G.frameToTexts df
    pure $ Display
      [ DisplayData MimeHtml . cs . Lucid.renderText
        $ template header rows
      ]

instance forall r c a. Renderable a => IHaskellDisplay (Rect r c a) where
--  display :: forall r c a. Rect r c a -> IO Display
  display sq = do
    let (header:rows) = case SQ.toTexts sq of
                          [] -> [["empty"]]
                          x  -> x
    --     emptyLabel :: a
    --     emptyLabel = panic "emptyLabel"

    --     _ = header :: [a]

    -- renderedHeader <- displayList header
    -- let _ = renderedHeader :: [Display]

    -- renderedRows <- mapM displayList rows
    -- let _ = renderedRows :: [[Display]]

    -- pure $ Display
    --   [ DisplayData MimeHtml . cs . Lucid.renderText
    --     $ template renderedHeader renderedRows
    --   ]
    -- panic "Rect.display"
    pure $ Display
      [ DisplayData MimeHtml . cs . Lucid.renderText
        $ template header rows
      ]

displayList :: forall a. IHaskellDisplay a => [a] -> IO [Display]
displayList = mapM display

instance ( Forall a Typeable
         , Forall a Unconstrained1
         , Forall a ToField
         , SNatI n
         , WellBehaved (Map (Vec n) a)
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
