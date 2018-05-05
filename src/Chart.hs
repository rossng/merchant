{-# OPTIONS_GHC -w #-}
module Chart where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

import Language
import Valuation
import Observable

mkChart = toFile def "mychart.svg" $ do
    layout_title .= "`One GBP` value"
    setColors [opaque blue, opaque red]
    plot (line "GBP" [oneValue [0,1..50]])

oneValue :: [Time] -> [(Double, Double)]
oneValue times = [ (fromIntegral t, expectedValues !! t) | t <- times ]
  where expectedValues = expectedValuePr $ valueProcess GBP 0 (one' GBP)