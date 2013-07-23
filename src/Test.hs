module Main where

import Control.Monad.Reader

import SampleData
import StaticHedges

import Math.Root.Finder
import Numeric.GSL.Minimization

x = runReader (upInDigital sampleSingleBarrierDigital) sampleEquity

main = do
  print x
