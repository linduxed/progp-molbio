module Profile where

import MolSeq

type ProfileMatrix = [[Double]]
data ProfileType = DNAProf | ProteinProf deriving (Show, Eq)

data Profile = Profile
    { profName     :: String
    , profSeqCount :: Int
    , profType     :: ProfileType
    , profMatrix   :: ProfileMatrix
    } deriving (Show, Eq)
