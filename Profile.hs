module Profile where

import MolSeq

data ProfileType = DNA | Protein deriving (Show, Eq)

data Profile = Profile
    { profName      :: String
    , profSequences :: Integer
    , profType      :: ProfileType
    , profMatrix    :: [[Double]]
    } deriving (Show, Eq)
