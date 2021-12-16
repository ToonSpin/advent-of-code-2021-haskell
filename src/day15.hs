import qualified Data.Map as Map
import Data.List

type Coords    = (Int, Int)
type Node      = (Int, Coords)
type Cavern    = Map.Map Coords Int
type NodeQueue = [Node]

expandLine :: Int -> [Int] -> [Int]
expandLine 1 xs = xs
expandLine n xs =
    let incremented = map (\ x -> 1 + (x `mod` 9)) xs
    in xs ++ expandLine (n - 1) incremented

expandH :: Int -> [[Int]] -> [[Int]]
expandH n matrix = map (expandLine n) matrix

expandMatrix :: Int -> [[Int]] -> [[Int]]
expandMatrix 1 matrix = matrix
expandMatrix n matrix = transpose $ expandH n $ transpose $ expandH n matrix

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

inputToCavern :: Int -> String -> Cavern
inputToCavern n = matrixToCavern . (expandMatrix n) . stringToMatrix

addSingleToQueue :: Node -> NodeQueue -> NodeQueue
addSingleToQueue n [] = [n]
addSingleToQueue n [x] = [min x n, max x n]
addSingleToQueue n (x:xs)
    | n > x = x : (addSingleToQueue n xs)
    | otherwise = (n:x:xs)

dedupQueue :: (Node -> Node -> Bool) -> NodeQueue -> NodeQueue
dedupQueue _ [] = []
dedupQueue _ [x] = [x]
dedupQueue f (x:xs) = x : filter (not . f x) xs

addToQueue :: [Node] -> NodeQueue -> NodeQueue
addToQueue ns queue =
    let match (_, p1) (_, p2) = p1 == p2
    in dedupQueue match $ foldr addSingleToQueue queue ns

getNeighbors :: Coords -> Cavern -> [Node]
getNeighbors (x, y) cavern =
    let ns = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        p2n _ Nothing     = []
        p2n p (Just cost) = [(cost, p)]
        query c p = p2n p $ Map.lookup p c
        in concat $ map (query cavern) ns

maxCoord :: Cavern -> Int
maxCoord cavern = maximum $ map snd $ Map.keys cavern

getMinimumCost' :: Coords -> NodeQueue -> Cavern -> Int
getMinimumCost' _ [] _ = error "Can't find a path"
getMinimumCost' dest ((costSoFar,start):queue) cavern
    | start == dest = costSoFar
    | otherwise =
        let neighbors      = getNeighbors start cavern
            newCavern      = Map.delete start cavern
            addCost (c, p) = (c + costSoFar, p)
            newQueue       = addToQueue (map addCost neighbors) queue
        in getMinimumCost' dest newQueue newCavern

getMinimumCost :: Cavern -> Int
getMinimumCost cavern =
    let start = head $ getNeighbors (0, -1) cavern
        queue = [(0, snd start)]
        maxC  = maxCoord cavern
    in getMinimumCost' (maxC, maxC) queue cavern

main :: IO ()
main = do
    contents <- getContents

    putStr "The minimum risk required to get through part of the cave: "
    print $ getMinimumCost $ inputToCavern 1 contents

    putStr "The minimum risk required to get through the entire cave: "
    print $ getMinimumCost $ inputToCavern 5 contents
