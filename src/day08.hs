import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

type Wire = Char
type Digit = [Wire]
data Segment = Top | TopLeft | TopRight | Middle | BottomLeft | BottomRight | Bottom deriving (Eq, Ord, Show, Enum)

data Entry = Entry { signalPattern :: [Digit]
                   , outputValue :: [Digit]
                   } deriving (Show)

type W2S = Map.Map Wire (Set.Set Segment)
type S2W = Map.Map Segment (Set.Set Wire)

data Mapping = Mapping { digits :: [Digit]
                       , segments :: W2S
                       , wires :: S2W
                       } deriving (Show)


digitsFromEntry :: Entry -> [Digit]
-- digitsFromEntry e = signalPattern e ++ outputValue e
digitsFromEntry e = signalPattern e

newMapping :: [Digit] -> Mapping
newMapping digits =
    let s = Map.fromList $ zip allWires $ repeat $ Set.fromList allSegments
        w = Map.fromList $ zip allSegments $ repeat $ Set.fromList allWires
    in Mapping { digits = digits, segments = s, wires = w }

exclusiveCombination :: Digit -> [Segment] -> Mapping -> Mapping
exclusiveCombination w s m =
    let wireSet    = Set.fromList w
        segmentSet = Set.fromList s
        doSegments wire segments
            | Set.member wire wireSet = Set.intersection segments segmentSet
            | otherwise               = Set.difference segments segmentSet
        doWires segment wires
            | Set.member segment segmentSet = Set.intersection wires wireSet
            | otherwise                     = Set.difference wires wireSet
        in Mapping { digits   = digits m
                   , segments = Map.mapWithKey doSegments $ segments m
                   , wires    = Map.mapWithKey doWires $ wires m
                   }

processOne :: Digit -> Mapping -> Mapping
processOne d m = exclusiveCombination d [TopRight, BottomRight] m

processOnes :: Mapping -> Mapping
processOnes m =
    let ones = filter ((== 2) . length) (digits m)
    in foldr processOne m ones

processSeven :: Digit -> Mapping -> Mapping
processSeven d m = exclusiveCombination d [Top, TopRight, BottomRight] m

processSevens :: Mapping -> Mapping
processSevens m =
    let sevens = filter ((== 3) . length) (digits m)
    in foldr processSeven m sevens

processFour :: Digit -> Mapping -> Mapping
processFour d m = exclusiveCombination d [TopLeft, TopRight, Middle, BottomRight] m

processFours :: Mapping -> Mapping
processFours m =
    let fours = filter ((== 4) . length) (digits m)
    in foldr processFour m fours

unambiguousMapping :: Mapping -> Bool
unambiguousMapping m =
    let unambSet s    = length s < 2
        unambSegments = all unambSet $ Map.elems $ segments m
        unambWires    = all unambSet $ Map.elems $ wires m
    in unambWires && unambSegments

allSegments :: [Segment]
allSegments = [Top, TopLeft, TopRight, Middle, BottomLeft, BottomRight, Bottom]

allWires :: [Wire]
allWires = "abcdefg"

getEntry :: String -> Entry
getEntry s =
    let (pattern, rest)  = splitAt 58 s
        (_, outputValue) = splitAt 3 rest
    in Entry { signalPattern = words pattern, outputValue = words outputValue }

getInput :: String -> [Entry]
getInput s = map getEntry $ lines s

unambigWires :: Digit -> Maybe (Set.Set Wire)
unambigWires x =
    case length x of 2 -> Just $ Set.fromList x
                     3 -> Just $ Set.fromList x
                     4 -> Just $ Set.fromList x
                     7 -> Just $ Set.fromList x
                     _ -> Nothing

unambigSegments :: Digit -> Maybe (Set.Set Segment)
unambigSegments x =
    case length x of 2 -> Just $ Set.fromList [TopRight, BottomRight]
                     3 -> Just $ Set.fromList [Top, TopRight, BottomRight]
                     4 -> Just $ Set.fromList [TopLeft, TopRight, Middle, BottomRight]
                     7 -> Just $ Set.fromList $ allSegments
                     _ -> Nothing

countUnambiguous :: [Entry] -> Int
countUnambiguous input =
    let digits    = concat $ map outputValue input
        isUnamb d = unambigSegments d /= Nothing
    in length $ filter isUnamb digits

solveEntry :: Mapping -> Mapping
solveEntry m = processOnes $ processSevens $ processFours m

main = do
    contents <- getContents
    let input = getInput $ contents

    print $ countUnambiguous input
    -- mapM print $ map solveEntry $ map newMapping $ map digitsFromEntry input
    print $ head input
    print $ solveEntry $ newMapping $ digitsFromEntry $ head input
    print $ solveEntry $ solveEntry $ solveEntry $ solveEntry $ solveEntry $ solveEntry $ newMapping $ digitsFromEntry $ head input
