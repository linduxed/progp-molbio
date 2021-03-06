module MolSeq where

import Data.Function (on)

type Name = String

nucleotides :: String
aminoacids  :: String
nucleotides = "ACGT"
aminoacids  = "ARNDCEQGHILKMFPSTWYV"

data Molecule = DNA | Protein deriving (Show, Eq)
data MolSeq = MolSeq
    { molName     :: String
    , molSequence :: String
    , molType     :: Molecule
    } deriving (Show)

string2seq :: String -> String -> MolSeq
string2seq inName inSequence = MolSeq inName inSequence (determineMolType inSequence)

determineMolType :: String -> Molecule
determineMolType inSequence
    | all (`elem` nucleotides) inSequence = DNA
    | all (`elem` aminoacids)  inSequence = Protein
    | otherwise                           = error "Sequence contains unknown letters."

seqDistance :: MolSeq -> MolSeq -> (Name, Name, Double)
seqDistance a b
    | not $ sameLength  a b                     = error "Sequences are not of same length."
    | not $ sameMolType a b                     = error "Can't compare DNA to a Protein."
    | molType a == DNA     && difference > 0.74 = (molName a, molName b, 3.3)
    | molType a == Protein && difference > 0.94 = (molName a, molName b, 3.7)
    | otherwise                                 = (molName a, molName b, calcDistance)
    where
        sequenceLength   = length $ molSequence a
        differentLetters = length $ filter (not . uncurry (==)) $ (zip `on` molSequence) a b
        difference       = ((/) `on` fromIntegral) differentLetters sequenceLength

        calcDistance
            | molType a == DNA = -3/4 * log (1 - 4/3 * difference)
            | otherwise        = -19/20 * log (1 - 20/19 * difference)

sameMolType :: MolSeq -> MolSeq -> Bool
sameMolType = (==) `on` molType

sameLength :: MolSeq -> MolSeq -> Bool
sameLength = (==) `on` (length . molSequence)
