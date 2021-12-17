data Cavern = Cavern [[Octopus]] Int Int
type Coords = (Int, Int)
data Octopus = Octopus Int Bool deriving (Show)

getInput :: String -> Cavern
getInput s =
    let lightLevel c    = read [c]
        charToOctopus c = Octopus (lightLevel c) False
    in Cavern (map (map charToOctopus) $ lines s) 0 0

flash :: Octopus -> Octopus
flash (Octopus lightLevel _) = Octopus lightLevel True

hasFlashed :: Octopus -> Bool
hasFlashed (Octopus _ f) = f

bumpLightLevel :: Octopus -> Octopus
bumpLightLevel (Octopus lightLevel flashed) = Octopus (lightLevel + 1) flashed

resetOctopus :: Octopus -> Octopus
resetOctopus (Octopus lightLevel _)
    | lightLevel > 9  = Octopus 0 False
    | otherwise       = Octopus lightLevel False

reset :: Cavern -> Cavern
reset (Cavern cavern _ total) =
    let count = length $ filter hasFlashed $ concat cavern
    in Cavern (map (map resetOctopus) cavern) count (total + count)

bump :: Cavern -> Cavern
bump (Cavern cavern thisTurn total) = Cavern (map (map bumpLightLevel) cavern) thisTurn total

flashCount :: Cavern -> Int
flashCount (Cavern _ _ f) = f

needsFlashing :: Octopus -> Bool
needsFlashing (Octopus lightLevel flashed)
    | lightLevel <= 9 = False
    | otherwise      = not flashed

mapOctopus :: Cavern -> (Octopus -> Octopus) -> Coords -> Cavern
mapOctopus (Cavern cavern  thisTurn total) f c =
    let mapSingle y (x, o)
            | (x, y) == c = f o
            | otherwise   = o
        mapRow _ (y, row) = map (mapSingle y) $ zip [0..] row
    in Cavern (map (mapRow f) $ zip [0..] cavern)  thisTurn total

mapOctopuses :: Cavern -> (Octopus -> Octopus) -> [Coords] -> Cavern
mapOctopuses cavern _ [] = cavern
mapOctopuses cavern f (c:cs) =
    let mapped = mapOctopus cavern f c
    in mapOctopuses mapped f cs

flashOctopuses :: Cavern -> [Coords] -> Cavern
flashOctopuses cavern cs =
    let toBump    = concat $ map neighbors cs
        flashed   = mapOctopuses cavern flash cs
    in mapOctopuses flashed bumpLightLevel toBump

flashUntilDone :: Cavern -> [Coords] -> Cavern
flashUntilDone cavern [] = cavern
flashUntilDone cavern cs =
    let flashedCavern = flashOctopuses cavern cs
        toFlash       = map fst $ enumerateToFlash flashedCavern
    in flashUntilDone flashedCavern toFlash

step :: Cavern -> Cavern
step cavern =
    let bumpedCavern  = bump cavern
        toFlash       = map fst $ enumerateToFlash bumpedCavern
    in  reset $ flashUntilDone bumpedCavern toFlash

neighbors :: Coords -> [Coords]
neighbors (x, y) =
    let xs
            | x == 0 = [x, x + 1]
            | x == 9 = [x - 1, x]
            | otherwise = [x - 1, x, x + 1]
        ys
            | y == 0 = [y, y + 1]
            | y == 9 = [y - 1, y]
            | otherwise = [y - 1, y, y + 1]
    in filter (/= (x, y)) ((\ p q -> (p, q)) <$> xs <*> ys)

enumerateOctopuses :: Cavern -> [(Coords, Octopus)]
enumerateOctopuses (Cavern cavern _ _) =
    let addY y (x, o)             = ((x, y), o)
        processRow (y, zippedRow) = map (addY y) zippedRow
    in concat $ map processRow $ zip [0..] $ map (zip [0..]) cavern

enumerateToFlash :: Cavern -> [(Coords, Octopus)]
enumerateToFlash cavern =
    let toFlash (_, o) = needsFlashing o
    in filter toFlash $ enumerateOctopuses cavern

main :: IO ()
main = do
    contents <- getContents
    let input               = getInput contents
        done (Cavern _ f _) = f == 100
        iterations          = iterate step input

    putStr "The number of flashes after 100 steps: "
    print $ flashCount $ head $ drop 100 $ iterations

    putStr "The number of steps until the flashes are in sync: "
    print $ length $ takeWhile (not . done) iterations
