import qualified Data.Set as Set

type Point = (Int, Int, Int)

data OnOff = On | Off deriving (Show, Eq)

data Cuboid = Cuboid { onOff :: OnOff
                     , xRange :: (Int, Int)
                     , yRange :: (Int, Int)
                     , zRange :: (Int, Int)
                     } deriving (Show)

parseRange :: String -> ((Int, Int), String)
parseRange input = ((read rawInt1, read rawInt2), rest)
    where rawRange = takeWhile (/= ',') $ drop 2 input
          rawInt1  = takeWhile (/= '.') rawRange
          rawInt2  = drop (length rawInt1 + 2) rawRange
          rest     = drop (length rawRange + 3) input

readOnOff :: String -> OnOff
readOnOff "on"  = On
readOnOff "off" = Off
readOnOff _     = error "Invalid input"

readCuboid :: String -> Cuboid
readCuboid line = Cuboid {
    onOff  = readOnOff rawOnOff,
    xRange = xRange,
    yRange = yRange,
    zRange = zRange }
    where ranges          = tail $ dropWhile (/= ' ') line
          (xRange, xTail) = parseRange ranges
          (yRange, yTail) = parseRange xTail
          (zRange, _)     = parseRange yTail
          rawOnOff        = takeWhile (/= ' ') line

expandConfinedRange :: (Int, Int) -> [Int]
expandConfinedRange range
    | maxr < -50 = []
    | minr > 50  = []
    | otherwise  = [confine minr..confine maxr]
    where minr      = fst range
          maxr      = snd range
          confine x = min 50 (max (-50) x)

getAllPoints :: Cuboid -> Set.Set Point
getAllPoints cuboid = Set.fromList [
    (x, y, z)
    | x <- [fst $ xRange cuboid..snd $ xRange cuboid]
    , y <- [fst $ yRange cuboid..snd $ yRange cuboid]
    , z <- [fst $ zRange cuboid..snd $ zRange cuboid]
    ]

getConfinedPoints :: Cuboid -> Set.Set Point
getConfinedPoints cuboid = Set.fromList [
    (x, y, z)
    | x <- expandConfinedRange (xRange cuboid)
    , y <- expandConfinedRange (yRange cuboid)
    , z <- expandConfinedRange (zRange cuboid)
    ]

applyCuboid :: Set.Set Point -> Cuboid -> Set.Set Point
applyCuboid points cuboid
    | onOff cuboid == On  = Set.union points cuboidPoints
    | onOff cuboid == Off = Set.difference points cuboidPoints
    where cuboidPoints = getConfinedPoints cuboid

main :: IO ()
main = do
    contents <- getContents
    let input = map readCuboid $ lines contents
    let partOnePoints = foldl applyCuboid Set.empty input
    print $ length partOnePoints
