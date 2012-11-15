module Profile (fromMolSeqs, profileDistance) where

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

{-
 - Matrices are transposed so that columns are more easily compared.
 -}
generateMatrix :: [MolSeq] -> ProfileMatrix
generateMatrix sequences = transpose $ map (generateRow seqLetters) (transpose onlyStrings) where
    onlyStrings = map molSequence sequences
    seqType     = molType $ head sequences
    seqLetters
        | seqType == DNA = nucleotides
        | otherwise      = aminoacids

generateRow :: String -> String -> [Double]
generateRow [] _ = []
generateRow (letter:letters) seqHeads = letterRatio : generateRow letters seqHeads where
    letterRatio = ((/) `on` fromIntegral) (length $ filter (== letter) seqHeads) (length seqHeads)

{-
 - Matrices are transposed so that columns are more easily compared.
 - The matrices are flattened with concat for easier processing with zipWith.
 - Since the provided matrices need to be of identical structure and sizes, the
 - values will be properly aligned for zipping.
 -}
profileDistance :: Profile -> Profile -> Double
profileDistance aProf bProf = sum $ map abs $ zipWith (-) flatMatrixA flatMatrixB where
    flatMatrixA = flatTransMat aProf
    flatMatrixB = flatTransMat bProf

    flatTransMat = concat . transpose . profMatrix
