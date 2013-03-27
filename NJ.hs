-- Imports and types{{{
module NJ where
-- TODO: when module is finished, only "neighbor" should be exported

import Distance
import Data.List (transpose, minimumBy)
import Data.Ord (comparing)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

type EdgeMap = Map.Map (String, String) Double
type NodeSet = Set.Set String
-- }}}
-- Matrix import functions {{{
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

{-
 - Non-pairs, the diagonal of the input matrix, are of no use in the
 - calculations, since the algorithm only cares for distances between different
 - nodes.
 -
 - Also, if the non-pairs aren't removed they will in many cases become the
 - lowest value available, being chosen in favour of real pairs.
 -}
removeNonPairs :: EdgeMap -> EdgeMap
removeNonPairs = Map.filterWithKey (\(x, y) _ -> x /= y)
-- }}}
-- Neighbor Joining algorithm {{{
neighbor :: [[DistanceTriplet]] -> (NodeSet, EdgeMap)
neighbor inMatrix = neighborLoop startingNodes firstDMap startingNodes noEdges where
    firstDMap     = removeNonPairs $ matrixToMap inMatrix
    startingNodes = mapNamesToSet firstDMap
    noEdges       = Map.empty :: EdgeMap

    neighborLoop unusedNodes distanceMap treeNodes treeEdges
        | Set.size unusedNodes > 3 = neighborLoop newUnusedNodes newDMap oldAndNewNodes newEdges
        | otherwise                = (treeNodes, connectRemainingThreeNodes distanceMap treeEdges)
        where
            (newDMap, newNode) = createDMapWithConnectingNode distanceMap

            newUnusedNodes = undefined -- Remove connected nodes, add connecting node.
            oldAndNewNodes = treeNodes `Set.union` newNode
            newEdges       = undefined -- Add edges to connecting node, remove edges to the connected two.

{-
 - The Wikipedia entry for the algorithm calculates a matrix for this step, but
 - since this code already uses Maps we'll stick with that.
 - If the code turns out to be slow for larger Maps it could be optimized by
 - pre-calculating the row sums for sumFilteredKeys.
 -}
calculateQMap :: EdgeMap -> EdgeMap
calculateQMap inMap = Map.mapWithKey qMatrixElemEquation inMap where
    qMatrixElemEquation (i, j) dist = (numberOfNames - 2) * dist - sumFilteredKeys i - sumFilteredKeys j

    numberOfNames        = fromIntegral $ Set.size $ mapNamesToSet inMap
    sumFilteredKeys name = sum $ Map.elems $ Map.filterWithKey (\(a, b) _ -> a == name || b == name) inMap

findLowestValueKey :: EdgeMap -> (String, String)
findLowestValueKey inMap = fst $ minimumBy (comparing snd) $ Map.toList inMap

createDMapWithConnectingNode :: EdgeMap -> (EdgeMap, NodeSet)
createDMapWithConnectingNode inMap = (newDMap, Set.singleton newNodeName) where
    (nA, nB)    = findLowestValueKey inMap
    newNodeName = "(" ++ nA ++ " - " ++ nB ++ ")" -- Might get pretty long.

    newDMap            = dMapWithoutAorB `Map.union` edgesFromOldToNew `Map.union` edgesFromNewToAorB
    dMapWithoutAorB    = inMap `Map.difference` edgesToAorB
    edgesToAorB        = Map.filterWithKey (\(x, y) _ -> any (`elem` [nA, nB]) [x, y]) inMap
    notAorB            = Set.toList $ mapNamesToSet dMapWithoutAorB
    edgesFromOldToNew  = Map.fromList $ map (\x -> ((newNodeName, x), oldToNewNodeDistance x)) notAorB
    edgesFromNewToAorB = newEdgeA `Map.union` newEdgeB

    newEdgeA      = Map.singleton (newNodeName, nA) edgeADistance
    newEdgeB      = Map.singleton (newNodeName, nB) edgeBDistance
    edgeADistance = (d nA nB / 2) + ((1 / (2 * numberOfNames)) * (sumFilteredKeys nA - sumFilteredKeys nB))
    edgeBDistance = d nA nB - edgeADistance

    numberOfNames          = fromIntegral $ Set.size $ mapNamesToSet inMap
    oldToNewNodeDistance x = (d nA x + d nB x - d nA nB) / 2
    sumFilteredKeys name   = sum $ Map.elems $ Map.filterWithKey (\(a, b) _ -> a == name || b == name) inMap
    d                      = lookupDistance inMap

-- This is a somewhat ugly way of finding the value, regardless of what
-- order the (String, String) has. There should only be one correct order.
lookupDistance :: EdgeMap -> String -> String -> Double
lookupDistance dMap a b = fromMaybe reversePair $ Map.lookup (a, b) dMap where
    reversePair = fromMaybe 0 $ Map.lookup (b, a) dMap

connectRemainingThreeNodes :: EdgeMap -> EdgeMap -> EdgeMap
connectRemainingThreeNodes = undefined
-- }}}
