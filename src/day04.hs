import Data.List

data BingoCell = Marked Int | Unmarked Int deriving Show
type BingoBoard = [[BingoCell]]

split :: Char -> String -> [String]
split _ [] = []
split c xs =
    let (first, second) = span (/= c) xs
    in first : (split c $ tail second)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

getDrawn :: String -> [Int]
getDrawn s = map read $ split ',' $ head $ lines s

getBingoBoards :: String -> [BingoBoard]
getBingoBoards input =
    let nonempty      = filter (not . null) $ drop 2 $ lines input
        intsFromRow s = map read $ words s
        intsToCells l = map (\ i -> Unmarked i) l
    in chunks 5 $ map intsToCells $ map intsFromRow nonempty

cellIsMarked :: BingoCell -> Bool
cellIsMarked (Marked _) = True
cellIsMarked (Unmarked _) = False

markCell :: Int -> BingoCell -> BingoCell
markCell _ (Marked i) = (Marked i)
markCell n (Unmarked i)
    | i == n = Marked i
    | otherwise = Unmarked i

mark :: Int -> BingoBoard -> BingoBoard
mark n board = map (map (markCell n)) board

boardIsWinning :: BingoBoard -> Bool
boardIsWinning board =
    let rowIsWinning r = all cellIsMarked r
    in any rowIsWinning board || any rowIsWinning (transpose board)

noBoardsAreWinning :: [BingoBoard] -> Bool
noBoardsAreWinning boards =
    let anyAreWinning = any boardIsWinning boards
    in not anyAreWinning

anyBoardsAreNonwinning :: [BingoBoard] -> Bool
anyBoardsAreNonwinning boards =
    let allAreWinning = all boardIsWinning boards
    in not allAreWinning

firstWinningBoard :: [BingoBoard] -> BingoBoard
firstWinningBoard boards = head $ filter boardIsWinning boards

firstNonwinningBoard :: [BingoBoard] -> BingoBoard
firstNonwinningBoard boards = head $ filter (not . boardIsWinning) boards

scoreValue :: BingoCell -> Int
scoreValue (Unmarked i) = i
scoreValue _ = 0

score :: Int -> BingoBoard -> Int
score lastDrawn board =
    let rowScore row = sum $ map scoreValue row
        totalScore   = sum $ map rowScore board
    in lastDrawn * totalScore

markAll :: [BingoBoard] -> Int -> [BingoBoard]
markAll xs n = map (mark n) xs

main :: IO ()
main = do
    contents <- getContents
    let boards      = getBingoBoards contents
        drawn       = getDrawn contents
        iteration   = scanl markAll boards drawn

        nonWinning  = takeWhile noBoardsAreWinning $ iteration
        nextNumber  = head $ drop (length nonWinning - 1) drawn
        firstToWin  = firstWinningBoard $ markAll (last nonWinning) nextNumber

        someWinning = takeWhile anyBoardsAreNonwinning $ iteration
        nextNumber2 = head $ drop (length someWinning - 1) drawn
        lastToWin   = mark nextNumber2 $ firstNonwinningBoard $ last someWinning

    putStr "The score of the first board to win: "
    print $ score nextNumber firstToWin

    putStr "The score of the last board to win: "
    print $ score nextNumber2 lastToWin
