module Distance where

import MolSeq
import Profile

class Distance a where
    distance :: a -> a -> Double

instance Distance MolSeq where
    distance a b = seqDistance a b

instance Distance Profile where
    distance a b = profileDistance a b

distanceMatrix :: Distance a => [a] -> [[Double]]
distanceMatrix xs = [ map (uncurry distance . (,) elems) xs | elems <- xs ]
