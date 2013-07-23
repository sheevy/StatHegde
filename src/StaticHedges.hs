module StaticHedges where

import Control.Lens
import Control.Monad.Reader
import Math.Root.Finder
import Numeric.GSL.Minimization

import BlackScholes
import Market

prepare f a b x | x < a || x > b = 1/0
                | otherwise      = f x

upInDigital :: SingleBarrierDigital -> Reader Equity Double
upInDigital option = do
  let b = view barrier option
  s <- asks $ view spot
  let t = view expiry1 option
  v <- asks $ view sigma
  let f [k] = prepare (\x -> (blackScholes True s x t 0.0 v) / (b - x)) 0 b k
  let (s, p) = minimize NMSimplex2 eps 1000 [minimum [1, b/10]] f [b/2]
  return $ head s

upInCall option = do
  let digital = forgetStrike option
  let b = view barrier2 option
  return 1.0
  
