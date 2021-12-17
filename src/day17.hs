type Target = ((Int, Int),(Int, Int))
type Coords = (Int, Int)
type Velocities = (Int, Int)

parseTarget :: String -> ((Int, Int),(Int, Int))
parseTarget xs =
    let xs'   = drop 15 (head $ lines xs)
        xPart = takeWhile (/= ',') xs'
        yPart = drop (length xPart + 4) xs'
        (xMinPart, xMaxPart) = span (/= '.') xPart
        (yMinPart, yMaxPart) = span (/= '.') yPart
        xMin = read xMinPart
        yMin = read yMinPart
        xMax = read $ drop 2 xMaxPart
        yMax = read $ drop 2 yMaxPart
    in ((xMin, xMax), (yMin, yMax))

coordsInTarget :: Target -> Coords -> Bool
coordsInTarget ((minX, maxX), (minY, maxY)) (x, y)
    = x >= minX && x <= maxX && y >= minY && y <= maxY

overshot :: Target -> Coords -> Bool
overshot ((_, maxX), (minY, _)) (x, y)
    = x > maxX || y < minY

stepVelocities :: Velocities -> Velocities
stepVelocities (vx, vy) = (max (vx - 1) 0, vy - 1)

stepCoords :: Velocities -> Coords -> Coords
stepCoords (vx, vy) (x, y) = (x + vx, y + vy)

canLand :: Target -> Coords -> Velocities -> Bool
canLand t (x, y) vs
    | coordsInTarget t (x, y) =  True
    | overshot t (x, y) = False
    | otherwise =
        let steppedV = stepVelocities vs
            steppedC = stepCoords vs (x, y)
        in canLand t steppedC steppedV

minVelocity :: Int -> Int
minVelocity maxX =
    let maxXF = fromIntegral maxX :: Float
    in ceiling $ sqrt (2 * maxXF) - 1

maxCoord :: Int -> Int
maxCoord v = v * (v + 1) `div` 2

main :: IO ()
main = do
    contents <- getContents

    let t@((minX, maxX), (minY, _)) = parseTarget contents
        minXV = minVelocity minX -- probe won't reach target if vx is less
        trials = [(vx, vy) | vx <- [minXV..maxX], vy <- [minY..(-minY)]]
        valid = filter (canLand t (0, 0)) $ trials

    putStr "The highest point the probe will reach is: "
    print $ maximum $ map (maxCoord . snd) $ valid

    putStr "The number of valid initial velocity values: "
    print $ length valid
