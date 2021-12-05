data Command = Command Char Int
data Position = Position Int Int Int

parseCommand :: [String] -> Command
parseCommand [command, param] = Command (head command) (read param)

executeCommandP1 :: Position -> Command -> Position
executeCommandP1 (Position depth distance aim) (Command command param) =
    case command of
        'f' -> Position depth (distance + param) aim
        'd' -> Position (depth + param) distance aim
        'u' -> Position (depth - param) distance aim

executeCommandP2 :: Position -> Command -> Position
executeCommandP2 (Position depth distance aim) (Command command param) =
    case command of
        'f' -> Position (depth + aim * param) (distance + param) aim
        'd' -> Position depth distance (aim + param)
        'u' -> Position depth distance (aim - param)

main = do
    contents <- getContents

    let input = map parseCommand $ map words $ lines contents
        getOutput = (\ (Position depth distance _) -> depth * distance)

    putStr "Position times depth for part 1: "
    putStrLn $ show $ getOutput $ foldl executeCommandP1 (Position 0 0 0) input

    putStr "Position times depth for part 2: "
    putStrLn $ show $ getOutput $ foldl executeCommandP2 (Position 0 0 0) input
