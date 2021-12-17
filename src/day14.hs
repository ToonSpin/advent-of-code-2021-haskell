import qualified Data.Map as Map

type Ruleset = Map.Map (Char, Char) Char
type Tallies = Map.Map (Char, Char) Int
type PolymerCount = Map.Map Char Int

parseRule :: Ruleset -> String -> Ruleset
parseRule m (x:y:rest) = Map.insert (x, y) (last rest) m
parseRule m _ = m

applyRule :: (Char, Char) -> Char -> (Tallies, Tallies) -> (Tallies, Tallies)
applyRule pair insertion (old, new) =
    let count = Map.findWithDefault 0 pair old
        p1    = (fst pair, insertion)
        p2    = (insertion, snd pair)
    in (old, Map.insertWith (+) p2 count $ Map.insertWith (+) p1 count $ new)

applyRules :: Ruleset -> Tallies -> Tallies
applyRules rules tallies = snd $ Map.foldrWithKey applyRule (tallies, Map.empty) rules

initTallies :: String -> Tallies -> Tallies
initTallies "" m = m
initTallies [_] m = m
initTallies (a:b:rest) m = initTallies (b:rest) $ Map.insertWith (+) (a, b) 1 m

countPolymers :: Char -> Tallies -> PolymerCount
countPolymers lastChar tallies =
    let countPolymer (c, _) count m = Map.insertWith (+) c count m
        counted = Map.foldrWithKey countPolymer Map.empty tallies
    in Map.insertWith (+) lastChar 1 counted

doSteps :: Char -> Ruleset -> Tallies -> Int -> PolymerCount
doSteps lastChar rules tallies rounds =
    let iterations = iterate (applyRules rules) tallies
        iterated   = head $ drop rounds $ iterations
    in countPolymers lastChar iterated

getAnswer :: PolymerCount -> Int
getAnswer m =
    let values = Map.elems m
    in (maximum values) - (minimum values)

main :: IO ()
main = do
    contents <- getContents

    let template = head $ lines contents
        lastChar = last $ template
        rules    = foldl parseRule Map.empty $ drop 2 $ lines contents
        tallies  = initTallies template Map.empty
        steps n  = doSteps lastChar rules tallies n

    putStr "The answer after 10 rounds: "
    print $ getAnswer $ steps 10

    putStr "The answer after 40 rounds: "
    print $ getAnswer $ steps 40
