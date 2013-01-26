module Distance where

import MolSeq
import Profile

class Distance a where
    distance :: a -> a -> Double

instance Distance MolSeq where
    distance = seqDistance

instance Distance Profile where
    distance = profileDistance

distanceMatrix :: Distance a => [a] -> [[Double]]
distanceMatrix xs = [ map (uncurry distance . (,) elems) xs | elems <- xs ]
