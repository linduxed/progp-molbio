-- Imports and types{{{
module NJ where
-- TODO: when module is finished, only "neighbor" should be exported

import Distance
import Data.List (transpose, minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map as Map

type EdgeMap = Map.Map (String, String) Double
type NodeSet = Set.Set String
-- }}}
-- Matrix import functions {{{
matrixColumnNamesToSet :: [[DistanceTriplet]] -> NodeSet
matrixColumnNamesToSet = Set.fromList . map (\(_,a,_) -> a) . head

mapNamesToSet :: EdgeMap -> NodeSet
mapNamesToSet = Set.fromList . unpairs . Map.keys where
    unpairs = concatMap (\(x, y) -> [x, y])

matrixToMap :: [[DistanceTriplet]] -> EdgeMap
matrixToMap inMatrix
    | (not . dMatIsSymmetric) inMatrix = error "Cannot use a non-symmetric distance matrix."
    | otherwise                        = Map.fromList $ formatMatrixForMap inMatrix

{-
 - Due to the distance matrix being formatted as (String, String, Double), with
 - the String values being flipped in the upper/lower triagle of the matrix,
 - symmetry is only tested for the Double value.
 - While fairly ugly, this code will only recieve distance matrices with
 - mentioned flipped Strings, so it shouldn't be a problem.
 -}
dMatIsSymmetric :: [[DistanceTriplet]] -> Bool
dMatIsSymmetric inMatrix = onlyNumValuesMatrix == transpose onlyNumValuesMatrix where
    onlyNumValuesMatrix = extractDoubles inMatrix
    extractDoubles      = map $ map (\(_,_,a) -> a)

formatMatrixForMap :: [[DistanceTriplet]] -> [((String, String), Double)]
formatMatrixForMap inMatrix = map (sortStringPair . groupStrings) flatMatrix where
    flatMatrix = concat inMatrix
    groupStrings (x, y, z) = ((x, y), z)
    sortStringPair ((x, y), z)
        | x < y     = ((x, y), z)
        | otherwise = ((y, x), z)
-- }}}
-- Neighbor Joining algorithm {{{
neighbor :: [[DistanceTriplet]] -> (NodeSet, EdgeMap)
neighbor inMatrix = neighborLoop startingNodes firstQMap startingNodes noEdges where
    firstQMap     = calculateQMap $ matrixToMap inMatrix
    startingNodes = matrixColumnNamesToSet inMatrix
    noEdges       = Map.empty :: EdgeMap

    neighborLoop unusedNodes qMap allNodes allEdges
        | Set.size unusedNodes > 3 = neighborLoop newUnusedNodes newQMap oldAndNewNodes newEdges
        | otherwise                = (allNodes, allEdges)
        where
            (newQMap, newNode) = createQMapWithConnectingNode qMap

            newUnusedNodes = undefined
            oldAndNewNodes = allNodes `Set.union` newNode
            newEdges       = undefined

{-
 - The Wikipedia entry for the algorithm calculates a matrix for this step, but
 - since this code already uses Maps we'll stick with that.
 - If the code turns out to be slow for larger Maps it could be optimized by
 - pre-calculating the row sums for sumFilteredKeys.
 -}
calculateQMap :: EdgeMap -> EdgeMap
calculateQMap distMap = Map.mapWithKey qMatrixElemEquation distMap where
    qMatrixElemEquation (i, j) dist
        | i == j    = 0 -- Only compare pairs.
        | otherwise = (numberOfNames - 2) * dist - sumFilteredKeys i - sumFilteredKeys j

    numberOfNames        = fromIntegral $ Set.size $ mapNamesToSet distMap
    sumFilteredKeys name = sum $ Map.elems $ Map.filterWithKey (\(a, b) _ -> a == name || b == name) distMap

findLowestValueKey :: EdgeMap -> (String, String)
findLowestValueKey inMap = fst $ minimumBy (comparing snd) $ Map.toList inMap

createQMapWithConnectingNode :: EdgeMap -> (EdgeMap, NodeSet)
createQMapWithConnectingNode inMap = (newQMap, Set.singleton newNodeName) where
    (lowNodeA, lowNodeB) = findLowestValueKey inMap
    newNodeName          = "(" ++ lowNodeA ++ " - " ++ lowNodeB ++ ")" -- Might get pretty long.

    newQMap             = (inMap `Map.difference` edgesToRemovedNodes) `Map.union` edgesToNewNode
    edgesToRemovedNodes = Map.filterWithKey (\(a, b) _ -> any (`elem` [lowNodeA, lowNodeB]) [a, b]) inMap
    edgesToNewNode      = undefined
-- }}}
