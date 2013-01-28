module EvolTree where

import Distance

type Name = String
type DistanceMatrix = [[Double]]

data Tree a = EmptyTree
            | Leaf a
            | Branch [Tree a]
