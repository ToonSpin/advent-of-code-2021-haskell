import Data.Char

data Number = Regular Int | Pair Number Number deriving (Eq)

instance Show Number where
    show (Regular x) = show x
    show (Pair x y) = "[" ++ show x ++ "," ++ show y ++ "]"

assertStartsWith :: Char -> String -> String
assertStartsWith c [] = error $ "Parse error, expected " ++ [c] ++ ", got nothing"
assertStartsWith c s@(x:_)
    | c == x = s
    | otherwise = error $ "Parse error, expected " ++ [c] ++ ", got " ++ [x] ++ " in " ++ s

parsePair :: String -> (Number, String)
parsePair xs =
    let (a, rest) = parseNumber xs
        (b, rest') = parseNumber $ drop 1 $ assertStartsWith ',' rest
    in (Pair a b, rest')

parseNumber :: String -> (Number, String)
parseNumber [] = error "Can't parse empty string"
parseNumber (x:xs)
    | isDigit x = (Regular $ read [x], xs)
    | x == '[' =
        let (p, rest) = parsePair xs
            rest' = drop 1 $ assertStartsWith ']' rest
        in (p, rest')
    | otherwise = error $ "Parse error: unexpected " ++ [x]

parseInput :: String -> [Number]
parseInput = map (fst . parseNumber) . lines 

split' :: Number -> (Number, Bool)
split' (Regular x)
    | x < 10 = (Regular x, False)
    | otherwise =
        let a = x `div` 2
            b = x - a
        in (Pair (Regular a) (Regular b), True)
split' (Pair x y) =
    let (x', done) = split' x
        (y', result') = if done then (y, True) else split' y
    in (Pair x' y', result')

split :: (Number, Bool) -> Number
split (x, True) = fst $ split' x
split (x, False) = x

numberLength :: Number  -> Int
numberLength (Regular _) = 1
numberLength (Pair x y) = numberLength x + numberLength y

applyToRegular' :: (Int -> Int) -> Int -> Number  -> Int -> Int -> Number
applyToRegular' f index n@(Regular x) left right
    | index < left = n
    | index >= right = n
    | index == left = Regular $ f x
    | otherwise = n
applyToRegular' f index n@(Pair x y) left right
    | index < left = n
    | index >= right = n
    | otherwise =
        let mid = left + numberLength x
            x' = applyToRegular' f index x left mid
            y' = applyToRegular' f index y mid right
        in Pair x' y'

applyToRegular :: (Int -> Int) -> Int -> Number -> Number
applyToRegular f index x = applyToRegular' f index x 0 (numberLength x)

coalesce :: Maybe a -> Maybe a -> Maybe a
coalesce (Just x) _ = Just x
coalesce Nothing y = y

ifNothing :: Maybe a -> b -> b -> b
ifNothing Nothing x _ = x
ifNothing (Just _) _ y = y

tryExplode :: Number -> Int -> Int -> Int -> (Number, Maybe (Int, Int, Int))
tryExplode x@(Regular _) _ _ _ = (x, Nothing)
tryExplode (Pair x y) level left right
    | level < 4 =
        let mid = left + numberLength x
            l = level + 1
            (x', r1) = tryExplode x l left mid
            (y', r2) = ifNothing r1 (tryExplode y l mid right) (y, Nothing)
        in (Pair x' y', coalesce r1 r2)
    | otherwise =
        let xpl (Regular a) (Regular b) = (Regular 0, Just (left, a, b))
            xpl _ _ = error "At level 4, pairs must both be regular"
        in xpl x y

afterTryExp :: (Number, Maybe (Int, Int, Int)) -> (Number, Bool)
afterTryExp (x, Nothing) = (x, True)
afterTryExp (x, (Just (index, a, b))) =
    let x'  = applyToRegular (+ a) (index - 1) x
        x'' = applyToRegular (+ b) (index + 1) x'
    in (x'', False)

explode :: (Number) -> (Number, Bool)
explode x = afterTryExp $ tryExplode x 0 0 (numberLength x)

reduce' :: Number -> Number
reduce' x = split $ explode x

reduce :: Number -> Number
reduce x = doUntilUnchanging reduce' x

add :: Number -> Number -> Number
add x y = reduce (Pair x y)

magnitude :: Number -> Int
magnitude (Regular x) = x
magnitude (Pair x y) = 3 * magnitude x + 2 * magnitude y

doUntilUnchanging :: Eq a => (a -> a) -> a -> a
doUntilUnchanging f x =
    let iterations = iterate f x
    in fst $ head $ dropWhile (\ (a, b) -> a /= b) $ zip iterations (drop 1 iterations)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (\ a -> (x, a)) xs ++ pairs xs

main :: IO ()
main = do
    contents <- getContents

    let input = parseInput contents

    putStr "The magnitude of the sum of all the numbers: "
    print $ magnitude $ foldl1 add input

    putStr "The highest magnitude out of the sums of all pairs: "
    print $ maximum $ map (\ (a, b) -> magnitude $ add a b ) $ pairs input
