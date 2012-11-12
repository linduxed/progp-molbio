module MolSeq (string2seq, seqDistance) where


data Molecule = DNA | Protein deriving (Show, Eq)
data MolSeq = MolSeq
    { molName     :: String
    , molSequence :: String
    , molType     :: Molecule
    } deriving (Show)

string2seq :: String -> String -> MolSeq

seqDistance :: MolSeq -> MolSeq -> Float
