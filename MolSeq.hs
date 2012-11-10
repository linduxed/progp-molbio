module MolSeq (string2seq, seqDistance) where

data Molecule = DNA | Protein

data MolSeq = MolSeq
    { name :: String
    , sequence :: String
    , molType :: Molecule
    } deriving (Show)

string2seq :: String -> String -> MolSeq

seqDistance :: MolSeq -> MolSeq -> Float
