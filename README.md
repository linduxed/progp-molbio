Haskell and Molecular biology
=============================

# Background

The two primary types of molecules that molecular biologists observe are DNA
and proteines. Both have a linear structure that you tend to represent as
strings, generally called "sequences". DNA has the well known structure of four
building blocks, the nucleotides A, C, T and G, which gives us DNA sequences
like ATTATCGGCTCT.

The abbreviations of the amino acids are represented by single letters, with
all of them being ARNDCEQGHILKMFPSTWYV (more details can be found [on
Wikipedia][1]).

  [1]: http://en.wikipedia.org/wiki/Amino_acid#Table_of_standard_amino_acid_abbreviations_and_properties

Lengths of DNA and amino acids can vary, sometimes being over a hundred symbols
long.

## Evolutionary distance between sequences

A common operation on a pair of sequences is to calculate their evolutionary
distance. A simple model called Jukes-Cantor discribes the distance _d(a,b)_
between two sequences _a_ and _b_ as __d(a,b)= -3/4 ln(1 - 4α/3)__, where α. is
the percentage of positions where the sequences differ.  
The formula doesn't work well if the sequences differ too much, so if α>0.74
then you often let d(a,b) = 3.3.

There's an almost identical model ("the Poisson model") for protein sequences
where you set the distance as __d(a,b) = -19/20 ln(1 - 20α/19)__ where
α ≤ 0.94, otherwise d(a,b) = 3.7.

# Tasks

* Create a data type called MolSeq for molecular sequences which defines the
  name, sequence and whether it's DNA or a protein.
* Implement ```string2seq ::String -> String -> MolSeq```, where the first
  argument should be a name and the second one a sequence. This function needs
  to be able to differentiate between DNA and proteins.
* Implement ```seqDistance :: MolSeq -> MolSeq -> Float```, which compares two
  DNA sequences or two protein sequences and returns their evolutionary
  distance. Comparing DNA with a protein should result in an error.

## Testing

The provided file ```molbio.hs``` contains a range of different sequences which
can be used for testing.
