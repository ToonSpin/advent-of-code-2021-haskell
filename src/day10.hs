import Data.List

data Chunk = Corrupted Char | Incomplete String deriving (Show)

matching :: Char -> Char
matching '(' = ')'
matching '[' = ']'
matching '{' = '}'
matching '<' = '>'
matching _   = 'x'

isOpening :: Char -> Bool
isOpening '(' = True
isOpening '[' = True
isOpening '{' = True
isOpening '<' = True
isOpening _   = False

buildChunk :: String -> String -> Chunk
buildChunk queue ""  = Incomplete queue
buildChunk queue@(q:qs) (x:xs)
    | isOpening x = buildChunk (matching x : queue) xs
    | x == q      = buildChunk qs xs
    | otherwise   = Corrupted x

chunkFromString :: String -> Chunk
chunkFromString (x:xs) = buildChunk [matching x] xs

isIncomplete :: Chunk -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _              = False

scoreCorrupted :: Chunk -> Int
scoreCorrupted (Incomplete _) = 0
scoreCorrupted (Corrupted c)  = case c of
              ')' -> 3
              ']' -> 57
              '}' -> 1197
              '>' -> 25137

scoreIncomplete :: Chunk -> Int
scoreIncomplete (Corrupted _)   = 0
scoreIncomplete (Incomplete xs) =
    let score x = case x of
              ')' -> 1
              ']' -> 2
              '}' -> 3
              '>' -> 4
        addScore acc x = (5 * acc) + x
    in foldl addScore 0 $ map score xs

pickFinalScore :: [Int] -> Int
pickFinalScore xs =
    let index  = (length xs) `div` 2
        sorted = sort xs
    in sorted !! index

main = do
    contents <- getContents
    let input = map chunkFromString $ lines contents
        (incomplete, corrupted) = partition isIncomplete input

    putStr "The total syntax error score of corrupted chunks: "
    print $ sum $ map scoreCorrupted $ corrupted

    putStr "The middle completion score of incomplete chunks: "
    print $ pickFinalScore $ map scoreIncomplete $ incomplete
