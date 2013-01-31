Haskell and Molecular biology
=============================

# Background

The two primary types of molecules that molecular biologists observe are DNA
and proteins. Both have a linear structure that you tend to represent as
strings, generally called "sequences". DNA has the well known structure of four
building blocks, the nucleotides A, C, T and G, which gives us DNA sequences
like ATTATCGGCTCT.

The abbreviations of the amino acids are represented by single letters, with
all of them being ARNDCEQGHILKMFPSTWYV (more details can be found [on
Wikipedia][1]).

  [1]: http://en.wikipedia.org/wiki/Amino_acid#Table_of_standard_amino_acid_abbreviations_and_properties

Lengths of DNA and amino acids can vary, sometimes being over a hundred symbols
long.

# Evolutionary distance between sequences

A common operation on a pair of sequences is to calculate their evolutionary
distance. A simple model called Jukes-Cantor describes the distance _d(a,b)_
between two sequences _a_ and _b_ as __d(a,b)= -3/4 ln(1 - 4α/3)__, where α is
the percentage of positions where the sequences differ.  
The formula doesn't work well if the sequences differ too much, so if α>0.74
then you often let d(a,b) = 3.3.

There's an almost identical model ("the Poisson model") for protein sequences
where you set the distance as __d(a,b) = -19/20 ln(1 - 20α/19)__, where
α ≤ 0.94, otherwise d(a,b) = 3.7.

# Profiles and sequences

Profiles are used to summarise the appearance of a range of related sequences.
They're interesting because studies have found that if you wish to look for
similarities, it's better to look with a profile than to look with singular
sequences.  
Normally profiles are used to summarise important _parts_ of of sequences but
in this exercise the task will be simplified to working with whole sequences.

## Profile implementation

A profile for DNA or protein sequences is a matrix _M=(m(i,j))_ where element
_m(i,j)_ is the frequency of the letter _j_ on position _i_. If all studied
sequences start with "A" then _m(1,A)=1_. If half of the sequences have "A" on
the second position and the other half has "C", then _m(2,A)=m(2,C)=0.5_.

This means that a DNA profile will have four rows (ACTG) while a protein
profile will have twenty (ARNDCEQGHILKMFPSTWYV), with columns matching the
length of the sequence.

## Distance between profiles

There's more than one way to measure the distance between two profiles. One is
to calculate the element-wise difference. If we let _M=(m(i,j))_ and
_M'=(m'(i,j))_ be profiles that span over _n_ positions. Their distance can
then be described as:

__d(M, M') = \sum i=1n \sum j \in {A,C,G,T} |m(i,j)-m'(i,j)|__

In other words, you sum both positions and different letters in each position.

# Calculation of distance matrices

Both in studies of molecular sequences and studies of profiles you occasionally
want to calculate the pairwise distances and summarize these in a so called
distance matrix.

This application will represent entries in the matrix with triplets of the
format *(Name 1, Name 2, Distance)*.

# Evolutionary trees

Evolutionary trees (also called phylogenies) have their use in enlightening us
as to how a species has come into existence. The knowledge of how certain genes
change over time can explain their purpose and function, which is of great use
in medicine.

To generate an evolutionary tree you need a set of sequences (DNA or proteins),
with the result being a graph where all the inner vertices have a degree of 3.

## Output format

Evolutionary trees are often written in a format called *Newick*. For details
on the exact workings of the format, refer to either [this Wikipedia
article](http://en.wikipedia.org/wiki/Newick_format) or [this
webpage](http://evolution.genetics.washington.edu/phylip/newicktree.html).

This application will not attempt to build trees with less than three input
sequences, so the smallest tree it will print out would be *(a,b,c)*.

## Algorithm

The most commonly used and well known algorithm for generating phylogenies is
Neighbor Joining, which is used in this application.

The workings of the algorithm are best explained by the corresponding
[Wikipedia article](http://en.wikipedia.org/wiki/Neighbor_joining).

# Tasks

## Sequences

* Create a data type called ```MolSeq``` for molecular sequences which defines the
  name, sequence and whether it's DNA or a protein.
* Implement ```string2seq :: String -> String -> MolSeq```, where the first
  argument should be a name and the second one a sequence. This function needs
  to be able to differentiate between DNA and proteins.
* Implement ```seqDistance :: MolSeq -> MolSeq -> Double```, which compares two
  DNA sequences or two protein sequences and returns their evolutionary
  distance. Comparing DNA with a protein should result in an error.

## Profiles

* Create a data type called ```Profile``` which should store the following
  information:
	* The profile, stored in a matrix.
	* Whether it's a DNA or a Protein profile.
	* How many sequences the profile is built from.
	* A name of the profile.
* Implement ```fromMolSeqs :: [MolSeq] -> Profile```. The profile name is
  arbitrary and may, for example, be obtained by using the name of the first
  sequence.
* Implement ```profileDistance :: Profile -> Profile -> Double```. The distance
  between two profiles _M_ and _M'_ should be measured with the the above
  mentioned formula _d(M, M')_.

## Distance matrices

* Implement the ```Distance``` typeclass and let MolSeq and Profile be
  instances of Distance. A function called ```distance``` must be implemented
  for all instances.
* Implement the function ```distanceMatrix``` inside ```Distance```, which
  takes a list of ```MolSeq``` or ```Profile``` as input and returns all pairs
  of distances, in the shape of above mentioned triplets.

## Evolutionary trees

* Implement a function called ```neighbor``` which takes output data from
  ```distanceMatrix``` and returns a string representing an evolutionary tree,
  using the Newick format, generated with the Neigbor Joining algorithm.
* The algorithm is to be implemented in a separate module called ```NJ```, with
  the only exported function in it being ```neighbor```.
* The data structures ```Data.Set``` and ```Data.Map``` need to be used in
  meaningful ways.

### Testing

The file ```tests.hs``` contains a range of different sequences which can be
used for testing.
Unit tests have been written for course provided reference values.
