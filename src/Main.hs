module Main where

import Control.Lens
import Control.Monad.Reader
import Data.Vector.Unboxed

import Market
import PDE
import SampleData
import StaticHedges

x = runReader (upInDigital sampleSingleBarrierDigital) sampleEquity
y = runReader (upInCall sampleSingleBarrierCall) sampleEquity
z = runReader (downInDigital sampleSingleBarrierDigital2) sampleEquity
w = runReader (downInCall sampleSingleBarrierCall2) sampleEquity

pde :: Double
pde = vec ! ns
  where
    vec = allSteps sampleEquity sampleGrid (view nt sampleGrid) samplePayoff
    spot' = view spot sampleEquity
    s' = view s sampleGrid
    dS' = view dS sampleGrid
    ns = round $ (spot' - s')/dS'

main = do
  print x
  print y
  print z
  print w
  print pde
