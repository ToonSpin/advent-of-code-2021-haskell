data Packet = Packet { version :: Int
                     , typeId :: Int
                     , payload :: Payload
                     } deriving Show
data Payload = Literal Int | Operator [Packet] deriving Show

hexDigitToBits :: Char -> [Int]
hexDigitToBits '0' = [0,0,0,0]
hexDigitToBits '1' = [0,0,0,1]
hexDigitToBits '2' = [0,0,1,0]
hexDigitToBits '3' = [0,0,1,1]
hexDigitToBits '4' = [0,1,0,0]
hexDigitToBits '5' = [0,1,0,1]
hexDigitToBits '6' = [0,1,1,0]
hexDigitToBits '7' = [0,1,1,1]
hexDigitToBits '8' = [1,0,0,0]
hexDigitToBits '9' = [1,0,0,1]
hexDigitToBits 'A' = [1,0,1,0]
hexDigitToBits 'B' = [1,0,1,1]
hexDigitToBits 'C' = [1,1,0,0]
hexDigitToBits 'D' = [1,1,0,1]
hexDigitToBits 'E' = [1,1,1,0]
hexDigitToBits 'F' = [1,1,1,1]
hexDigitToBits _ = error "Invalid hex digit"

getBitsFromInput :: String -> [Int]
getBitsFromInput input =
    let hexDigits = head $ lines input
    in concat $ map hexDigitToBits hexDigits

bitsToInt :: [Int] -> Int
bitsToInt bits =
    let addBit acc b = 2 * acc + b
    in foldl addBit 0 bits

literalBits :: [Int] -> ([Int], [Int])
literalBits (0:xs) = (take 4 xs, drop 4 xs)
literalBits (1:xs) =
    let (extraBits, rest) = literalBits (drop 4 xs)
    in (take 4 xs ++ extraBits, rest)
literalBits _      = error "Invalid bit"

parseAllPackets :: [Int] -> [Packet]
parseAllPackets [] = []
parseAllPackets xs =
    let (p, rest) = parsePacket xs
    in p : parseAllPackets rest

parseSequential :: Int -> [Int] -> ([Packet], [Int])
parseSequential 0 xs = ([], xs)
parseSequential n xs =
    let (p, rest) = parsePacket xs
        (pTail, restTail) = parseSequential (n - 1) rest
    in (p : pTail, restTail)

parseLiteral :: [Int] -> (Payload, [Int])
parseLiteral xs =
    let (bits, rest) = literalBits xs
    in (Literal $ bitsToInt bits, rest)

parseOperator :: [Int] -> (Payload, [Int])
parseOperator (0:xs) =
    let len     = bitsToInt $ take 15 xs
        packets = parseAllPackets $ take len $ drop 15 xs
    in (Operator packets, drop (15 + len) xs)
parseOperator (1:xs) =
    let count           = bitsToInt $ take 11 xs
        (packets, rest) = parseSequential count $ drop 11 xs
    in (Operator packets, rest)
parseOperator _ = error "Invalid bit"

parsePayload :: Int -> [Int] -> (Payload, [Int])
parsePayload tid xs
    | tid == 4  = parseLiteral xs
    | otherwise = parseOperator xs

parsePacket :: [Int] -> (Packet, [Int])
parsePacket (v1:v2:v3:t1:t2:t3:xs) =
    let ver       = bitsToInt $ [v1, v2, v3]
        tid       = bitsToInt $ [t1, t2, t3]
        (pay, rest) = parsePayload tid xs
        packet = Packet {
            version = ver,
            typeId  = tid,
            payload = pay
        }
    in (packet, rest) 
parsePacket _ = error "Invalid packet"

versionSum :: Packet -> Int
versionSum (Packet v _ (Literal _)) = v
versionSum (Packet v _ (Operator ps)) =
    let subPackets = sum $ map versionSum ps
    in v + subPackets

evaluate :: Packet -> Int
evaluate (Packet _ _ (Literal v)) = v
evaluate (Packet _ tid (Operator ps)) =
    let p = head ps
        q = head $ tail ps
    in case tid of
        0 -> sum $ map evaluate ps
        1 -> product $ map evaluate ps
        2 -> minimum $ map evaluate ps
        3 -> maximum $ map evaluate ps
        5 -> if evaluate p > evaluate q then 1 else 0
        6 -> if evaluate p < evaluate q then 1 else 0
        7 -> if evaluate p == evaluate q then 1 else 0
        _ -> error "Unknown operator"

main :: IO ()
main = do
    contents <- getContents

    let bits = getBitsFromInput $ contents

    putStr "The sum of the versions of all the packets: "
    print $ sum $ map versionSum $ parseAllPackets bits

    putStr "The result of the evaluation of the BITS expression: "
    print $ evaluate $ fst $ parsePacket bits
