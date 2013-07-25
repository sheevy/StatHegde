module StaticHedges where

import Control.Lens
import Control.Monad.Reader
--import Data.Packed.Matrix
--import Math.Root.Finder
--import Numeric.GSL.Minimization

import BlackScholes
import Extrema
import Market

prepare f a b x | x < a || x > b = 1/0
                | otherwise      = f x

upInDigital :: SingleBarrierDigital -> Reader Equity (Double, Double)
upInDigital option =
  let b = view barrier option in
  let t = view expiry1 option in
  do
    s <- asks $ view spot
    v <- asks $ view sigma
    let f k = (blackScholes True s k t 0.0 v) / (b - k)
    return $ minimize f 0 b

upInCall :: SingleBarrierStrike -> Reader Equity Double
upInCall option =
  let b = view barrier2 option in
  let k = view strike2 option in
  do
    (s, p) <- upInDigital $ forgetStrike option
    if s <= k
      then
        do
          spot <- asks $ view spot
          let t = view expiry2 option
          v <- asks $ view sigma
          return $ blackScholes True spot k t 0.0 v 
      else
        return $ (b-k) * p

downInDigital :: SingleBarrierDigital -> Reader Equity (Double, Double)
downInDigital option =
  let b = view barrier option in
  let t = view expiry1 option in
  do
    s <- asks $ view spot
    v <- asks $ view sigma
    let f k = (blackScholes False s k t 0.0 v) / (k - b)
    return $ minimize f b (10*b) --FIXME: hardcoded value of 10 (should be infinity)

downInCall :: SingleBarrierStrike -> Reader Equity Double
downInCall option =
  let b = view barrier2 option in
  let k = view strike2 option in
  let t = view expiry2 option in
  do
    spot <- asks $ view spot
    v <- asks $ view sigma
    let put d = (d - k)/(d - b) * blackScholes False spot b t 0.0 v
    let call d = (k - b)/(d - b) * blackScholes True spot d t 0.0 v
    let f d = put d + call d
    let min = minimize f k (10*k)
    return $ snd min

