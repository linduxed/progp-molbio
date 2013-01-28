module Distance where

import MolSeq
import Profile

type DistanceTriplet = (Name, Name, Double)

class Distance a where
    distance :: a -> a -> DistanceTriplet

instance Distance MolSeq where
    distance = seqDistance

instance Distance Profile where
    distance = profileDistance

makeDistanceMatrix :: Distance a => [a] -> [[DistanceTriplet]]
makeDistanceMatrix xs = [ map (uncurry distance . (,) elems) xs | elems <- xs ]
