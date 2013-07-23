module Main where

import Control.Lens

import PDE
import SampleData

main :: IO ()
main = do
  let show' n = show $ allSteps sampleMarket sampleEquity sampleGrid (view nt sampleGrid `div` 10 * n) samplePayoff
  Prelude.mapM_ (print . (Prelude.drop 10) . show') [50]
