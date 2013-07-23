module BlackScholes (blackScholes) where

import Data.Random

stdNormalCdf :: Double -> Double
stdNormalCdf = cdf StdNormal

blackScholes isCall s x t r v =
  if isCall then call else put
  where
    call = s * stdNormalCdf d1 - x*exp (-r*t) * stdNormalCdf d2
    put  = x * exp (-r*t) * stdNormalCdf (-d2) - s * stdNormalCdf (-d1)
    d1   = (log(s/x) + (r+v*v/2)*t) / vt
    d2   = d1 - vt
    vt   = v * sqrt t
