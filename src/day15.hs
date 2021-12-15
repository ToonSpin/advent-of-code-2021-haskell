import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

type Coords = (Int, Int)
type Node   = (Int, Coords)
type Cavern = Map.Map Coords Int

inputToCavern :: Int -> String -> Cavern
inputToCavern n = matrixToCavern . (expandMatrix n) . stringToMatrix

stringToMatrix :: String -> [[Int]]
stringToMatrix contents =
    let charToInt c  = read [c]
        lineToInts s = map charToInt s
    in map lineToInts $ lines contents

matrixToCavern :: [[Int]] -> Cavern
matrixToCavern matrix =
    let addEntry (x, y, cost) m = Map.insert (x, y) cost m
        lineToCoords (y, xs) = [(x, y, cost) | (x, cost) <- zip [0..] xs]
        enumerated = concat $ map lineToCoords $ zip [0..] $ matrix
    in foldr addEntry Map.empty enumerated

expandH :: Int -> [[Int]] -> [[Int]]
expandH n matrix =
    let linePairs xs  = zip [0..] $ take n $ repeat xs
        wrapRisk r = ((r - 1) `mod` 9) + 1
        expandMulPair (f, xs) = map (wrapRisk . (+) f) xs
        expandLine xs = concat $ map expandMulPair $ linePairs xs
    in map expandLine matrix

expandMatrix :: Int -> [[Int]] -> [[Int]]
expandMatrix n matrix = transpose $ expandH n $ transpose $ expandH n matrix

getNeighbors :: Coords -> Cavern -> [Node]
getNeighbors (x, y) cavern =
    let ns = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        p2n _ Nothing     = []
        p2n p (Just cost) = [(cost, p)]
        query c p = p2n p $ Map.lookup p c
        in concat $ map (query cavern) ns

dedupNodeList :: [Node] -> [Node]
dedupNodeList [] = []
dedupNodeList [x] = [x]
dedupNodeList ((c,p):xs) = (c, p) : filter (\ (_, q) -> q /= p) xs

addToQueue :: [Node] -> Set.Set Node -> Set.Set Node
addToQueue ns queue =
    let newSet = foldr Set.insert queue ns
    in Set.fromList $ dedupNodeList $ Set.toList newSet

getMinimumCost' :: Coords -> Set.Set Node -> Cavern -> Int
getMinimumCost' dest queue cavern
    | start == dest = costSoFar
    | otherwise =
        let neighbors      = getNeighbors start cavern
            newCavern      = Map.delete start cavern
            addCost (c, p) = (c + costSoFar, p)
            newQueue'      = addToQueue (map addCost neighbors) newQueue
        in getMinimumCost' dest newQueue' newCavern
    where ((costSoFar, start), newQueue) = Set.deleteFindMin queue

maxCoord :: Cavern -> Int
maxCoord cavern = maximum $ map snd $ Map.keys cavern

getMinimumCost :: Cavern -> Int
getMinimumCost cavern =
    let start = head $ getNeighbors (0, -1) cavern
        queue = Set.fromList [(0, snd start)]
        maxC  = maxCoord cavern
    in getMinimumCost' (maxC, maxC) queue cavern

main :: IO ()
main = do
    contents <- getContents

    putStr "The minimum risk required to get through part of the cave: "
    print $ getMinimumCost $ inputToCavern 1 contents

    putStr "The minimum risk required to get through the entire cave: "
    print $ getMinimumCost $ inputToCavern 5 contents
