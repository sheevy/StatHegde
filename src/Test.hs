module Main where

import Control.Monad.Reader

import SampleData
import StaticHedges

import Math.Root.Finder
import Numeric.GSL.Minimization

x = runReader (upInDigital sampleSingleBarrierDigital) sampleEquity
y = runReader (upInCall sampleSingleBarrierCall) sampleEquity
z = runReader (downInDigital sampleSingleBarrierDigital2) sampleEquity
w = runReader (downInCall sampleSingleBarrierCall2) sampleEquity

main = do
  print x
  print y
  print z
