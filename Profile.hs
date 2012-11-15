module Profile where

import MolSeq
import Data.List (transpose)
import Data.Function (on)

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
    seqType = allSequencesType sequences

allSequencesType :: [MolSeq] -> ProfileType
allSequencesType seqList
    | all ((== DNA) . molType)     seqList = DNAProf
    | all ((== Protein) . molType) seqList = ProteinProf
    | otherwise                            = error "Can't create profile for more than one type of sequence."

generateMatrix :: [MolSeq] -> ProfileMatrix
generateMatrix sequences = genLoop onlyStrings where
    onlyStrings = map molSequence sequences
    seqType     = molType $ head sequences

    genLoop seqStrings = zipWith (++) headColumn tailColumns where
        seqHeads = map head seqStrings
        seqTails = map tail seqStrings

        headColumn
            | seqType == DNA = transpose [generateRow seqHeads nucleotides]
            | otherwise      = transpose [generateRow seqHeads aminoacids]

        tailColumns
            | all null seqTails = repeat []
            | otherwise         = genLoop seqTails

generateRow :: String -> String -> [Double]
generateRow _ [] = []
generateRow headsToCompare (letter:letters) = letterRatio : generateRow headsToCompare letters where
    letterRatio = ((/) `on` fromIntegral) (length $ filter (== letter) headsToCompare) (length headsToCompare)
