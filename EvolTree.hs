-- Imports, types and data {{{
module EvolTree where

import Distance
import NJ
import qualified Data.Map as Map
-- }}}
-- Newick formatting {{{
{-
 - Due to how the neighbor function builds the edge names, the first String is
 - always the branching node, while the second one may or may not be
 - a branching node.
 -}
newick :: [[DistanceTriplet]] -> String
newick inMatrix = outString where
    outString = "(" ++ treeLoop edgeList True ++ ");"
    edgeList = map fst $ Map.toList $ neighbor inMatrix

    treeLoop [] _ = []
    treeLoop (x:xs) nextToParen
        | bothNodesAreBranches = "(" ++ treeLoop notBranchNodes True ++ ")" ++ treeLoop branchNodes False
        | nextToParen          = snd x ++ treeLoop xs False
        | otherwise            = ", " ++ snd x ++ treeLoop xs False
        where
            isBranch             = elem ';' -- The neighbor function uses the ';' as a separator in branch names.
            bothNodesAreBranches = isBranch (fst x) && isBranch (snd x)
            branchNodes          = filter onCurrentBranch xs
            notBranchNodes       = filter (not . onCurrentBranch) xs
            onCurrentBranch y    = fst y == fst x
-- }}}
