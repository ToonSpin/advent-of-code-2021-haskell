countIncreasingValues :: [Int] -> Int
countIncreasingValues [_] = 0
countIncreasingValues (x1:x2:xs)
    | x1 < x2 = 1 + countIncreasingValues (x2:xs)
    | otherwise = countIncreasingValues (x2:xs)

slidingWindows :: Int -> [Int] -> [Int]
slidingWindows n xs
    | length xs < n = []
    | otherwise = (sum $ take n xs) : slidingWindows n (tail xs)

main = do
    contents <- getContents
    let input = map read (lines contents)
    putStr "The number of increasing depth measurements: "
    putStrLn $ show $ countIncreasingValues input
    putStr "The number of increasing sums of sliding windows: "
    putStrLn $ show $ countIncreasingValues $ slidingWindows 3 input
