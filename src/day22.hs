import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.List as List

type Range = (Int, Int)
type Point = (Int, Int, Int)

data OnOff = On | Off deriving (Show, Eq)

data Cuboid = Cuboid { xRange :: Range
                     , yRange :: Range
                     , zRange :: Range
                     } deriving (Show)

data Rule = Rule { onOff :: OnOff
                 , cuboid :: Cuboid
                 } deriving (Show)

parseRange :: String -> (Range, String)
parseRange input = ((read rawInt1, read rawInt2), rest)
    where rawRange = takeWhile (/= ',') $ drop 2 input
          rawInt1  = takeWhile (/= '.') rawRange
          rawInt2  = drop (length rawInt1 + 2) rawRange
          rest     = drop (length rawRange + 3) input

readOnOff :: String -> OnOff
readOnOff "on"  = On
readOnOff "off" = Off
readOnOff _     = error "Invalid input"

readRule :: String -> Rule
readRule line = Rule {
    onOff  = readOnOff rawOnOff,
    cuboid = Cuboid {
        xRange = xRange,
        yRange = yRange,
        zRange = zRange }
    }
    where ranges          = tail $ dropWhile (/= ' ') line
          (xRange, xTail) = parseRange ranges
          (yRange, yTail) = parseRange xTail
          (zRange, _)     = parseRange yTail
          rawOnOff        = takeWhile (/= ' ') line

isOutsideInit :: Range -> Bool
isOutsideInit r =
    let fstOutside = (fst r) > 50
        sndOutside = (snd r) < (-50)
    in  or [fstOutside, sndOutside]

confineRangeToInit :: Range -> Range
confineRangeToInit r = (max (-50) (fst r), min 50 (snd r))

confineCuboidToInit :: Cuboid -> Maybe Cuboid
confineCuboidToInit cuboid
    | isOutsideInit $ xRange cuboid = Nothing
    | isOutsideInit $ yRange cuboid = Nothing
    | isOutsideInit $ zRange cuboid = Nothing
    | otherwise = Just Cuboid {
        xRange  = xrc,
        yRange  = yrc,
        zRange  = zrc }
        where xrc = confineRangeToInit $ xRange cuboid
              yrc = confineRangeToInit $ yRange cuboid
              zrc = confineRangeToInit $ zRange cuboid

confineRuleToInit :: Rule -> Maybe Rule
confineRuleToInit r
    | Maybe.isNothing confinedCuboid = Nothing
    | otherwise = Just Rule {
            onOff  = onOff r,
            cuboid = Maybe.fromJust confinedCuboid
        }
    where confinedCuboid = confineCuboidToInit $ cuboid r

confineRulesToInit :: [Rule] -> [Rule]
confineRulesToInit rs = Maybe.mapMaybe confineRuleToInit rs

contains :: Range -> Range -> Bool
contains r s
    | fst s < fst r = False
    | snd s > snd r = False
    | otherwise = True

splitRangeHighEdge :: Int -> Range -> [Range]
splitRangeHighEdge x r
    | x < fst r = [r]
    | x >= snd r = [r]
    | otherwise = [(fst r, x), (x + 1, snd r)]

splitRangeLowEdge :: Int -> Range -> [Range]
splitRangeLowEdge x r
    | x <= fst r = [r]
    | x > snd r = [r]
    | otherwise = [(fst r, x - 1), (x, snd r)]

splitRange :: Range -> Range -> [Range]
splitRange splitter splittee =
    let firstSplit = splitRangeLowEdge (fst splitter) splittee
        in concat $ map (splitRangeHighEdge (snd splitter)) firstSplit

splitCuboid :: Cuboid -> Cuboid -> [Cuboid]
splitCuboid splitter splittee = [
        Cuboid { xRange = xr, yRange = yr, zRange = zr }
        | xr <- splitRange (xRange splitter) (xRange splittee)
        , yr <- splitRange (yRange splitter) (yRange splittee)
        , zr <- splitRange (zRange splitter) (zRange splittee)
    ]

areAdjacent :: Range -> Range -> Bool
areAdjacent r s
    | (fst s) - (snd r) == 1 = True
    | (fst r) - (snd s) == 1 = True
    | otherwise              = False

joinAdjacent :: Range -> Range -> Range
joinAdjacent r s
    | (fst s) - (snd r) == 1 = (fst r, snd s)
    | (fst r) - (snd s) == 1 = (fst s, snd r)
    | otherwise              = error "not adjacent"

canJoinCuboids :: Cuboid -> Cuboid -> Bool
canJoinCuboids c d
    | and [areAdjacent (xRange c) (xRange d), yRange c == yRange d, zRange c == zRange d] = True
    | and [xRange c == xRange d, areAdjacent (yRange c) (yRange d), zRange c == zRange d] = True
    | and [xRange c == xRange d, yRange c == yRange d, areAdjacent (zRange c) (zRange d)] = True
    | otherwise                                       = False
    -- where cx = xRange c == xRange d
    --       cy = yRange c == yRange d
    --       cz = zRange c == zRange d

joinCuboids :: Cuboid -> Cuboid -> Cuboid
joinCuboids c d
    | and [areAdjacent (xRange c) (xRange d), cy, cz]
        = Cuboid {
            xRange  = joinAdjacent (xRange c)  (xRange d),
            yRange  = yRange c,
            zRange  = zRange c }
    | and [cx, areAdjacent (yRange c) (yRange d), cz]
        = Cuboid {
            xRange  = xRange c,
            yRange  = joinAdjacent (yRange c)  (yRange d),
            zRange  = zRange c }
    | and [cx, cy, areAdjacent (zRange c) (zRange d)]
        = Cuboid {
            xRange  = xRange c,
            yRange  = yRange c,
            zRange  = joinAdjacent (zRange c)  (zRange d) }
    | otherwise = error "not adjacent"
    where cx = xRange c == xRange d
          cy = yRange c == yRange d
          cz = zRange c == zRange d

shouldJoin :: [Cuboid] -> Bool
shouldJoin cs
    | length cs < 100 = False
    | otherwise       = True

joinPair :: [Cuboid] -> ([Cuboid], Bool)
joinPair [] = ([], False)
joinPair [c] = ([c], False)
joinPair (c:cs)
    | length matches == 0 =
        let (joined, found) = joinPair cs
        in (c:joined, found)
    | otherwise = (joinCuboids c (head matches) : ((tail matches) ++ nonmatches), True)
    where (matches, nonmatches) = List.partition (canJoinCuboids c) cs

containsCuboid :: Cuboid -> Cuboid -> Bool
containsCuboid c d = and [
        (xRange c) `contains` (xRange d),
        (yRange c) `contains` (yRange d),
        (zRange c) `contains` (zRange d)
    ]

joinPossible :: [Cuboid] -> [Cuboid]
joinPossible cs
    | found     = joinPossible joined
    | otherwise = joined
    where (joined, found) = joinPair cs

applyRule :: [Cuboid] -> Rule -> [Cuboid]
applyRule cs (Rule onOff cuboid)
    | onOff == On = cuboid:processed
    | otherwise   = processed
    where afterSplit = concat $ map (splitCuboid cuboid) cs
          filtered   = filter (not . (containsCuboid cuboid)) afterSplit
          processed  = joinPossible filtered

rangeSpan :: Range -> Int
rangeSpan r = ((snd r) - (fst r)) + 1

volume :: Cuboid -> Int
volume cuboid = (rangeSpan $ xRange cuboid) * (rangeSpan $ yRange cuboid) * (rangeSpan $ zRange cuboid)

main :: IO ()
main = do
    contents <- getContents
    let input = map readRule $ lines contents

    let partOne = foldl applyRule [] (confineRulesToInit input)
    print $ sum (map volume partOne)

    let partTwo = foldl applyRule [] input
    print $ sum (map volume partTwo)
