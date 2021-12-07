countIncreasing :: Int -> [Int] -> Int
countIncreasing windowSize depths =
    let diffs xs = zipWith (-) (drop windowSize xs) xs
    in length $ filter (> 0) $ diffs depths

main = do
    contents <- getContents
    let input = map read $ lines contents

    putStr "The number of increasing depth measurements: "
    print $ countIncreasing 1 input

    putStr "The number of increasing sums of sliding windows: "
    print $ countIncreasing 3 input
