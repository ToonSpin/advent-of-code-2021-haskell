import Data.Char

getInput :: String -> [Int]
getInput [] = []
getInput (',':xs) = getInput xs
getInput xs =
    let (d, rest) = span isDigit xs
    in (read d) : getInput rest

fuelCost :: (Int -> Int) -> [Int] -> Int -> Int
fuelCost f xs n = sum $ map (f . abs . (n -)) xs

partTwo :: Int -> Int
partTwo x = x * (x + 1) `div` 2

main :: IO ()
main = do
    contents <- getContents
    let input = getInput $ head $ lines contents

    putStr "The least amount of fuel at constant burn rate: "
    print $ minimum $ map (fuelCost id input) $ [0..length input]

    putStr "The least amount of fuel at quadratic burn rate: "
    print $ minimum $ map (fuelCost partTwo input) $ [0..length input]
