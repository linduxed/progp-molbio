-- Imports {{{
module NJ where
-- TODO: when module is finished, only "neighbor" should be exported

import Distance
import Data.List (transpose)
import qualified Data.Set as Set
import qualified Data.Map as Map
-- }}}
-- Matrix import functions {{{
matrixColumnNamesToSet :: [[DistanceTriplet]] -> Set.Set String
matrixColumnNamesToSet = Set.fromList . map (\(_,a,_) -> a) . head

mapNamesToSet :: Map.Map (String, String) Double -> Set.Set String
mapNamesToSet = Set.fromList . unpairs . Map.keys where
    unpairs = concatMap (\(x, y) -> [x, y])

matrixToMap :: [[DistanceTriplet]] -> Map.Map (String, String) Double
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
neighbor :: [[DistanceTriplet]]
neighbor = undefined
-- }}}
