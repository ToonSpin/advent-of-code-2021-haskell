import Data.Char

data Point = Point Int Int deriving (Eq, Show)
data Fold = Horizontal Int | Vertical Int deriving (Show)

parsePoint :: String -> Point
parsePoint s =
    let x = takeWhile (/= ',') s
        l = 1 + (length x)
        y = drop l s
    in Point (read x) (read y)

parsePoints :: String -> [Point]
parsePoints s = map parsePoint $ filter isPoint $ lines s
    where isPoint s = not (null s) && isDigit (head s)

parseFold :: String -> Fold
parseFold s =
    let coord = s !! 11
        value = read $ drop 13 s
        toFold 'x' v = Vertical v
        toFold _ v   = Horizontal v
    in toFold coord value

parseFolds :: String -> [Fold]
parseFolds s = map parseFold $ filter isFold $ lines s
    where isFold s = not (null s) && (head s) == 'f'

foldCoordinate :: Int -> Int -> Int
foldCoordinate coord x = coord - abs (coord - x)

foldPoint :: Point -> Fold -> Point
foldPoint (Point p q) (Horizontal y) = Point p (foldCoordinate y q)
foldPoint (Point p q) (Vertical x) = Point (foldCoordinate x p) q

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : (dedup $ filter (/= x) xs)

maxX :: [Point] -> Int
maxX [(Point x _)] = x
maxX ((Point x _):ps) = maximum [x, maxX ps]

maxY :: [Point] -> Int
maxY [(Point _ y)] = y
maxY ((Point _ y):ps) = maximum [y, maxY ps]

pointsToLines :: [Point] -> [String]
pointsToLines ps =
    let mx = maxX ps
        my = maxY ps
        char x y = if (Point x y) `elem` ps then '#' else ' '
        line y = [char x y | x <- [0..mx]]
    in map line [0..my]

main = do
    contents <- getContents
    let points       = parsePoints contents
        folds        = parseFolds contents
        applyFirst p = foldPoint p (head folds)
        applyAll p   = foldl foldPoint p folds

    putStr "The number of visible dots after folding the page once: "
    print $ length $ dedup $ map applyFirst $ points
    putStrLn $ unlines $ pointsToLines $ dedup $ map applyAll $ points
