import qualified Data.Set as Set
import Data.List

type Floor = [[Int]]
type Coords = (Int, Int)
type Basin = Set.Set Coords

width :: Floor -> Int
width f = length $ head f

height :: Floor -> Int
height f = length f

getInput :: String -> Floor
getInput s =
    let c2i x = read [x]
    in map (map c2i) $ lines s

neighbors :: Floor -> Coords -> [Coords]
neighbors f (x, y) =
    let xs
            | x == 0 = [x, x + 1]
            | x == (width f) - 1 = [x - 1, x]
            | otherwise = [x - 1, x, x + 1]
        ys
            | y == 0 = [y, y + 1]
            | y == (height f) - 1 = [y - 1, y]
            | otherwise = [y - 1, y, y + 1]
        horiz = map (\ x -> (x, y)) xs
        vert = map (\ y -> (x, y)) ys
    in filter (/= (x, y)) (horiz ++ vert)

getHeightAt :: Floor -> Coords -> Int
getHeightAt f (x, y) = (f !! y) !! x

isLowPoint :: Floor -> Coords -> Bool
isLowPoint f (x, y) =
    let pointHeight = getHeightAt f (x, y)
    in all (> pointHeight) $ map (getHeightAt f) $ neighbors f (x, y)

allCoords :: Floor -> [Coords]
allCoords f =
    let mkCoords = (\ x y -> (x, y))
        xs = [0..(width f) - 1]
        ys = [0..(height f) - 1]
    in  mkCoords <$> xs <*> ys

buildBasin :: Floor -> Set.Set Coords -> [Coords] -> Basin
buildBasin f areas [] = Set.empty
buildBasin f areas (cur:queue) =
    let nb        = neighbors f cur
        found     = filter (\ n -> n `Set.member` areas) nb
        newQueue  = queue ++ found
        newAreas  = Set.difference areas $ Set.fromList nb 
    in Set.insert cur $ buildBasin f newAreas newQueue

findBasin :: Floor -> Set.Set Coords -> Basin
findBasin f areas =
    let element   = Set.elemAt 0 areas
        initAreas = Set.delete element areas
    in buildBasin f initAreas [element]

findBasins :: Floor -> Set.Set Coords -> [Basin]
findBasins f areas
    | Set.null areas = []
    | otherwise =
        let basin = findBasin f areas
        in basin : (findBasins f $ Set.difference areas basin)

getBasins :: Floor -> [Basin]
getBasins f =
    let isNine c = getHeightAt f c == 9
        noNines  = Set.fromList $ filter (not . isNine) $ allCoords f
    in findBasins f noNines

main = do
    contents <- getContents
    let input      = getInput contents
        lowPoints  = filter (isLowPoint input) $ allCoords input
        basinSizes = map Set.size $ getBasins input

    putStr "The sum of the risk levels of all the low points: "
    print $ (+) (length lowPoints) $ sum $ map (getHeightAt input) lowPoints

    putStr "The product of the sizes of the three largest basins: "
    print $ product $ take 3 $ reverse $ sort $ basinSizes
