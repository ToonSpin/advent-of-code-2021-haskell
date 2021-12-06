parseFish :: String -> [Int]
parseFish "" = []
parseFish (',':xs) = parseFish xs
parseFish (x:xs) = read [x] : parseFish xs

parsedToCounts :: [Int] -> [Int]
parsedToCounts xs =
    let count xs n = length $ filter (== n) xs
    in map (count xs) [0..8]

tick :: [Int] -> [Int]
tick xs =
    let numNew   = head xs
        ticked   = tail xs ++ [numNew]
        newSixes = (ticked !! 6) + numNew
    in  take 6 ticked ++ [newSixes] ++ drop 7 ticked

ticks :: Int -> [Int] -> [Int]
ticks n xs = head $ drop n $ iterate tick xs

main = do
    contents <- getContents
    let input = parsedToCounts $ parseFish $ head $ lines contents

    putStr "After 80 days: "
    putStrLn $ show $ sum $ ticks 80 input

    putStr "After 256 days: "
    putStrLn $ show $ sum $ ticks 256 input
