module Extrema (minimize, maximize) where

import Data.Graph.Inductive.Query.Monad (mapSnd)
import Data.Packed.Matrix
import Math.Root.Finder
import qualified Numeric.GSL.Minimization as M

bound :: (Ord a, Fractional b) => (a -> b) -> a -> a -> a -> b
bound f a b x | x < a || x > b = 1/0 -- remove this hack. replace it approperiate type class
              | otherwise      = f x

minimize :: (Double -> Double)-> Double -> Double -> (Double, Double)
minimize f a b =
  (head s, (flipud p) @@> (0,1))
  where
    fBounded [x] = bound f a b x
    mid = (a+b)/2/(1+eps)
    r = (b-a)/2
    (s, p) = M.minimize M.NMSimplex2 eps 1000 [r] fBounded [mid]

maximize :: (Double -> Double)-> Double -> Double -> (Double, Double)
maximize f a b =
  mapSnd (0-) min
  where
    min = minimize ((0-) . f) a b
