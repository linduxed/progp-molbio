module Evol where

import MolSeq
import Profile

class Evol a where
    distance :: a -> a -> Double

instance Evol MolSeq where
    distance a b = seqDistance a b

instance Evol Profile where
    distance a b = profileDistance a b

distanceMatrix :: Evol a => [a] -> [[Double]]
distanceMatrix xs = [ map (uncurry distance . (,) elems) xs | elems <- xs ]
