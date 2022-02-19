module IHaskell.Display.Tdf.Chart
  ( chartExample
  ) where

import Graphics.Rendering.Chart.Easy
import Control.Monad.Trans.State.Lazy ( State
                                      , StateT
                                      )

titles :: [String]
titles = [ "Cash", "Equity" ]

values :: [ (String, [Double]) ]
values =
  [ ("Jun", [20, 45])
  , ("Jul", [45, 30])
  , ("Aug", [30, 20])
  , ("Sep", [10, 40])
  , ("Oct", [20, 50])
  ]

-- chartExample :: EC (Layout PlotIndex Double) CState
chartExample :: StateT (Layout PlotIndex Double) (State CState) ()
chartExample = do
  layout_title .= "Example Bars"
  layout_title_style . font_size .= 10
  layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
  plot
    . fmap plotBars
    . bars titles
    . addIndexes
    . map snd
    $ values

