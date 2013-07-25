{-# LANGUAGE TemplateHaskell #-}

module PDE where

import Control.Lens
import Control.Monad.Reader
import Data.Vector.Unboxed
import Data.Vector.Unboxed as U

import Market

data Grid =
  Grid { _s    :: Double
       , _dS   :: Double
       , _nS   :: Int
       , _t    :: Double
       , _dt   :: Double
       , _nt   :: Int
       } deriving Show

makeLenses ''Grid

-- FIXME: fit the grid to equity spot
makeDBGrid :: Equity -> DoubleBarrier -> Int -> Grid
makeDBGrid equity db nS =
  Grid s dS nS t dt nt
  where
    s            = lowB
    dS           = (highB - lowB) / fromIntegral nS
    t            = _expiry db
    nt           = ceiling $ t / dtConstraint
    dt           = t / fromIntegral nt
    dtConstraint = 0.1 / vol / vol / nS' / nS'
    nS'          = fromIntegral nS
    lowB         = _lowBarrier db
    highB        = _highBarrier db
    vol          = _sigma equity

call :: Double -> Double -> Double
call k s = max 0.0 (s-k)

data Data =
  Data { _equity :: Equity
       , _grid   :: Grid
       } deriving Show

makeLenses ''Data

ph :: Vector Double -> Int -> Reader Data Double
ph v n = do
  let get = asks . view
  dt <- get $ grid.dt
  dS <- get $ grid.dS
  return $ phi' v n
    where
      phi' v n | n == 0               = 0
               | n == U.length v - 1  = 0

phi :: Equity -> Grid -> Vector Double -> Int -> Double
phi equity grid v n = phi' v n
  where
    phi' v n | n == 0               = 0
             | n == U.length v - 1  = 0
             | otherwise            = v!n - dt'*theta
                                      
    dt'   = view dt grid
    dS'   = view dS grid
    spot' = view s grid + (fromIntegral n) * dS'
    vol'  = view sigma equity
    delta = (v!(n+1) - v!(n-1)) / 2 / dS'
    gamma = (v!(n+1) - 2*v!n + v!(n-1)) / dS' / dS'
    theta = -0.5*vol'*vol'*spot'*spot'*gamma -- since r == 0

oneStep :: Equity -> Grid -> Vector Double -> Vector Double
oneStep e g v = U.map (phi e g v) (enumFromN 0 (U.length v))

allSteps :: Equity -> Grid -> Int -> Vector Double -> Vector Double
allSteps e g = allSteps' (oneStep e g) -- (nt sampleGrid)
  where
    allSteps' f n v | n == 0 = v
                    | n >= 1 = allSteps' f (n-1) (f v)



