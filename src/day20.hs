type Image = ([[Bool]], Bool)
type Coords = (Int, Int)

parse :: String -> ([Bool], Image)
parse input =
    let mapped = map (map (== '#')) $ lines input
        algorithm = head mapped
        image = (drop 2 mapped, False)
    in (algorithm, image)

withinBounds :: Coords -> Image -> Bool
withinBounds (x, y) (matrix, _) =
    let w = length $ head matrix
        h = length matrix
    in (x >= 0) && (y >= 0) && (x < w) && (y < h)

getValue :: Image -> Coords -> Bool
getValue i@(matrix, def) (x, y)
    | withinBounds (x,y) i = (matrix !! y) !! x
    | otherwise = def

getAlgorithmValue :: [Bool] -> Int -> Bool
getAlgorithmValue algorithm index
    | index < 0 = error "Index too small"
    | index >= length algorithm = error $ "Index too large (" ++ (show index) ++ ")"
    | otherwise = algorithm !! index

getOutputPixel :: Coords -> Image -> [Bool] -> Bool
getOutputPixel (x, y) i algorithm =
    let neighbors = getNeighbors (x, y)
        b2i b     = if b then 1 else 0
        f acc c   = 2 * acc + c
        index     = foldl1 f $ map (b2i . (getValue i)) neighbors
    in getAlgorithmValue algorithm index

getNewDefault :: Image -> [Bool] -> Bool
getNewDefault i a = getOutputPixel (-10, -10) i a

getNeighbors :: Coords -> [Coords]
getNeighbors (x, y) = [(p, q) | q <- [y-1..y+1], p <- [x-1..x+1]]

enhance :: [Bool] -> Image -> Image
enhance algorithm image@(matrix, _) =
    let w = length $ head matrix
        h = length matrix
        newDef = getNewDefault image algorithm
        makeRow y = [getOutputPixel (x, y) image algorithm | x <- [-1..w]]
        newMatrix = [makeRow y | y <- [-1..h]]
    in (newMatrix, newDef)

countLit :: Image -> Int
countLit (_, True) = error "Infinite number of lit pixels"
countLit (matrix, False) = sum $ map (length . (filter id)) matrix

main :: IO ()
main = do
    contents <- getContents

    let (algorithm, inputImage) = parse contents
    let iterations = iterate (enhance algorithm) inputImage

    putStr "The number of lit pixels after 2 enhancements: "
    print $ countLit $ head $ drop 2 iterations

    putStr "The number of lit pixels after 50 enhancements: "
    print $ countLit $ head $ drop 50 iterations
