module StaticHedges where

import Control.Lens
import Control.Monad.Reader
import Data.Packed.Matrix
import Math.Root.Finder
import Numeric.GSL.Minimization

import BlackScholes
import Market

prepare f a b x | x < a || x > b = 1/0
                | otherwise      = f x

upInDigital :: SingleBarrierDigital -> Reader Equity (Double, Double)
upInDigital option = do
  let b = view barrier option
  s <- asks $ view spot
  let t = view expiry1 option
  v <- asks $ view sigma
  let f [k] = prepare (\x -> (blackScholes True s x t 0.0 v) / (b - x)) 0 b k
  let (s, p) = minimize NMSimplex2 eps 1000 [minimum [1, b/10]] f [b/2]
  return (head s, (flipud p) @@> (0,1))

upInCall :: SingleBarrierCall -> Reader Equity Double
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
  
downInCall = undefined
