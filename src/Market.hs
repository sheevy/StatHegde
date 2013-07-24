{-# LANGUAGE TemplateHaskell #-}

module Market where

import Control.Lens

data Market =
  Market
  { _r :: Double
  } deriving Show

data Equity =
  Equity
  { _spot  :: Double
  , _sigma :: Double
  } deriving Show

-- move to separate module
data SingleBarrierDigital =
  SingleBarrierDigital
  { _barrier :: Double
  , _expiry1 :: Double
  }

data SingleBarrierStrike =
  SingleBarrierStrike
  { _barrier2 :: Double
  , _expiry2  :: Double
  , _strike2  :: Double
  }

data DoubleBarrier =
  DoubleBarrier
  { _strike      :: Double
  , _lowBarrier  :: Double
  , _highBarrier :: Double
  , _expiry      :: Double
  } deriving Show

makeLenses ''Market
makeLenses ''Equity
makeLenses ''SingleBarrierDigital
makeLenses ''SingleBarrierStrike
makeLenses ''DoubleBarrier

forgetStrike :: SingleBarrierStrike -> SingleBarrierDigital
forgetStrike option =
  SingleBarrierDigital
  { _barrier = view barrier2 option
  , _expiry1 = view expiry2 option
  }
