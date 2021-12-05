import Data.Char
import Data.List
import qualified Data.Map as Map

data Point = Point Int Int deriving (Eq, Ord)
type Line = (Point, Point)
type OceanFloor = Map.Map Point Int

parsePoint :: String -> Point
parsePoint s =
    let (s1, s2) = span isDigit s
    in Point (read s1) (read $ tail s2)

parseLine :: String -> Line
parseLine s =
    let (s1, s2) = break isSpace s
    in (parsePoint s1, parsePoint $ drop 4 s2)

isDiagonal :: Line -> Bool
isDiagonal (Point x1 y1, Point x2 y2) = x1 /= x2 && y1 /= y2

-- This assumes x1 /= x2 || y1 /= y2, because makeRange will return an infinite
-- list if m == n
expand :: Line -> [Point]
expand (Point x1 y1, Point x2 y2) =
    let makeRange m n = [m, (m + signum (n - m)) .. n]
        xs = makeRange x1 x2
        ys = makeRange y1 y2
    in map (\ (x, y) -> Point x y) $ zip xs ys

markPoint :: Point -> OceanFloor -> OceanFloor
markPoint p m = Map.insertWith (+) p 1 m

markPoints :: [Point] -> OceanFloor -> OceanFloor
markPoints ps m = foldr markPoint m ps

countDuplicateElems :: [Line] -> Int
countDuplicateElems input =
    let expanded   = map expand $ input
        oceanFloor = foldr markPoints Map.empty expanded
    in length $ filter (> 1) $ Map.elems $ oceanFloor

main = do
    contents <- getContents
    let input = map parseLine $ lines contents

    putStr "Overlapping points among nondiagonal lines: "
    putStrLn $ show $ countDuplicateElems $ filter (not . isDiagonal) input

    putStr "Overlapping points among all lines: "
    putStrLn $ show $ countDuplicateElems input
