import qualified Data.Set as Set

type Digit = Set.Set Char
type DigitSolution = [Digit]

digitsFromLine :: String -> [Digit]
digitsFromLine line = map Set.fromList $ filter (/= "|") $ words line

isSupersetOf :: Digit -> Digit -> Bool
isSupersetOf a b = b `Set.isSubsetOf` a

firstOfLength :: Int -> [Digit] -> Digit
firstOfLength n xs = head $ filter ((== n) . length) xs

solve :: [Digit] -> DigitSolution
solve xs =
    let eight = Set.fromList "abcdefg"
        four  = firstOfLength 4 xs
        seven = firstOfLength 3 xs
        one   = firstOfLength 2 xs
        three = firstOfLength 5 $ filter (one `Set.isSubsetOf`) xs
        six   = firstOfLength 6 $ filter (not . (one `Set.isSubsetOf`)) xs
        nine  = firstOfLength 6 $ filter (three `Set.isSubsetOf`) xs
        two   = firstOfLength 5 $ filter (not . (nine `isSupersetOf`)) xs
        five  = firstOfLength 5 $ filter (six `isSupersetOf`) xs
        zero  = firstOfLength 6 $ filter (not . (five `Set.isSubsetOf`)) xs
    in [zero, one, two, three, four, five, six, seven, eight, nine]

countUnambiguous :: [Digit] -> Int
countUnambiguous line =
    let isUnamb digit = case length digit of
            2 -> True
            3 -> True
            4 -> True
            7 -> True
            _ -> False
    in length $ filter isUnamb $ drop 10 line

digitValue :: DigitSolution -> Digit -> Char
digitValue solution digit = case lookup digit $ zip solution "0123456789" of
                                Nothing -> error "Can't find digit in solution"
                                Just v -> v

outputValue :: [Digit] -> Int
outputValue line =
    let solution = solve line
        values   = map (digitValue solution) $ drop 10 line
    in read values

main :: IO ()
main = do
    contents <- getContents

    let digitLists = map digitsFromLine $ lines contents

    putStr "The number of times a 1, 4, 7, or 8 appears in the output values: "
    print $ sum $ map countUnambiguous $ digitLists

    putStr "The sum of all the output values: "
    print $ sum $ map outputValue $ digitLists
