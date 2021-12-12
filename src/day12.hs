import Data.Char
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set

type Passage = (String, String)
type Path = [String]
type CaveSystem = Map.Map String [String]
type VisitCounter = (Bool, Set.Set String)

revert :: Passage -> Passage
revert (from, to) = (to, from)

addReverse :: [Passage] -> [Passage]
addReverse [] = []
addReverse (p:ps) = revert p : p : addReverse ps

toPassage :: String -> Passage
toPassage s =
    let from = takeWhile (/= '-') s
        l    = 1 + (length from)
        to   = drop l s
    in (from, to)

addToSystem :: Passage -> CaveSystem -> CaveSystem
addToSystem passage system =
    let (from, to) = passage
    in Map.insertWith (++) from [to] system

getInput :: String -> CaveSystem
getInput s =
    let passages = addReverse $ map toPassage $ lines s
    in foldr addToSystem Map.empty passages

visitCave :: String -> VisitCounter -> VisitCounter
visitCave cave (dup, visited)
    | isUpper $ head cave = (dup, visited)
    | cave `Set.notMember` visited = (dup, Set.insert cave visited)
    | otherwise = (True, visited)

isUnvisited :: String -> VisitCounter -> Bool
isUnvisited "start" _ = False
isUnvisited cave (False, visited) = True
isUnvisited cave (True, visited) = cave `Set.notMember` visited

findPaths :: CaveSystem -> VisitCounter -> String -> String -> [Path]
findPaths system visited to from
    | to == from = [[from]]
    | otherwise =
        let newVisited = visitCave from $ visited
            eligible c = c /= from && isUnvisited c newVisited
            adjacent   = fromJust $ Map.lookup from system
            nextFroms  = filter eligible adjacent
            findFrom c = findPaths system newVisited to c
        in map (from :) $ concat $ map findFrom nextFroms

main = do
    contents <- getContents
    let input = getInput contents

    putStr "The number of paths that visit small caves at most once: "
    print $ length $ findPaths input (True, Set.empty) "end" "start"

    putStr "The number of paths with at most one small cave visited twice: "
    print $ length $ findPaths input (False, Set.empty) "end" "start"
