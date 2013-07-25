module SampleData where

import Control.Lens
import Data.Vector.Unboxed as U

import Market
import PDE

--sampleMarket :: Market
--sampleMarket = Market 0

sampleEquity :: Equity
sampleEquity = Equity 100 0.2

sampleDB :: DoubleBarrier
sampleDB = DoubleBarrier 100 80 120 1

n :: Int
n = 100

sampleGrid :: Grid
sampleGrid = makeDBGrid sampleEquity sampleDB n

samplePrice :: Vector Double
samplePrice = enumFromStepN b dS' (nS'+1)
  where
    b   = view lowBarrier sampleDB
    dS' = view dS sampleGrid
    nS' = view nS sampleGrid

samplePayoff :: Vector Double
samplePayoff = payoff // [(U.length payoff - 1, 0)]
  where
    payoff = U.map (call . _strike $ sampleDB) samplePrice

sampleSingleBarrierDigital :: SingleBarrierDigital
sampleSingleBarrierDigital =
  SingleBarrierDigital
  { _expiry1 = 1.0
  , _barrier = 120
  }

sampleSingleBarrierDigital2 :: SingleBarrierDigital
sampleSingleBarrierDigital2 =
  SingleBarrierDigital
  { _expiry1 = 1.0
  , _barrier = 90
  }

sampleSingleBarrierCall :: SingleBarrierStrike
sampleSingleBarrierCall =
  SingleBarrierStrike
  { _expiry2  = 1.0
  , _barrier2 = 120
  , _strike2  = 100
  }

sampleSingleBarrierCall2 :: SingleBarrierStrike
sampleSingleBarrierCall2 =
  SingleBarrierStrike
  { _expiry2  = 1.0
  , _barrier2 = 80
  , _strike2  = 100
  }  
