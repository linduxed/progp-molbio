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

fromMolSeqs :: [MolSeq] -> Profile
fromMolSeqs sequences = Profile name sequenceCount seqType (generateMatrix sequences) where
    name = show sequenceCount ++ "seq " ++ show seqType
    sequenceCount = length sequences
    seqType = checkAllSequencesType sequences

    checkAllSequencesType seqList
        | all ((== DNA) . molType)     seqList = DNAProf
        | all ((== Protein) . molType) seqList = ProteinProf
        | otherwise                            = error "Can't create profile for more than one type of sequence."
