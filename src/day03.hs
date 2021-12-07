import Data.List

mostCommonDigit :: String -> Char
mostCommonDigit input =
    let zeroes = length $ filter (== '0') input
    in  if (zeroes * 2) > length input then '0' else '1'

invertDigit :: Char -> Char
invertDigit c = if c == '1' then '0' else '1'

leastCommonDigit :: String -> Char
leastCommonDigit = invertDigit . mostCommonDigit

-- readBin isn't present in my Numeric module for some reason
binToInt :: String -> Int
binToInt s = foldl (\ acc d -> (acc * 2) + read [d]) 0 s

partTwo :: [String] -> (String -> Char) -> Int -> String
partTwo [s] _ _ = s
partTwo input getFilterDigit position =
    let transposed = transpose input
        toFilter   = getFilterDigit $ transposed !! position
        filtered   = filter (\ s -> s !! position == toFilter) input
    in partTwo filtered getFilterDigit (position + 1)

main = do
    contents <- getContents

    let input       = lines contents
        gamma       = map mostCommonDigit $ transpose input
        epsilon     = map invertDigit $ gamma
        o2Generator = partTwo input mostCommonDigit 0
        co2Scrubber = partTwo input leastCommonDigit 0

    putStr "Gamma times epsilon: "
    print $ binToInt gamma * binToInt epsilon

    putStr "Oxygen generator times CO2 scrubber: "
    print $ binToInt o2Generator * binToInt co2Scrubber
