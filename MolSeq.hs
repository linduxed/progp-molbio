module MolSeq (string2seq, seqDistance) where


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
    where
        nucleotides = "ACGT"
        aminoacids  = "ARNDCEQGHILKMFPSTWYV"

seqDistance :: MolSeq -> MolSeq -> Float
