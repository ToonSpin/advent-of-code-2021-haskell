data Instruction = Forward | Down | Up
data Command = Command Instruction Int
data Position = Position Int Int Int

parseCommand :: [String] -> Command
parseCommand [command, param] =
    let instruction = case command of
            "forward" -> Forward
            "down"    -> Down
            "up"      -> Up
            _         -> error $ "Error parsing instruction: " ++ command
    in Command instruction (read param)
parseCommand _                = error "Parse error"

descend :: Int ->  Position -> Position
descend x (Position depth distance aim) = Position (depth + x) distance aim

descendWithAim :: Int ->  Position -> Position
descendWithAim x pos@(Position _ _ aim) = descend (x * aim) pos

ascend :: Int ->  Position -> Position
ascend x = descend (-x)

forward :: Int ->  Position -> Position
forward x (Position depth distance aim) = Position depth (distance + x) aim

adjustAim :: Int ->  Position -> Position
adjustAim x (Position depth distance aim) = Position depth distance (aim + x)

executeCommandP1 :: Position -> Command -> Position
executeCommandP1 pos (Command Forward param) = forward param pos
executeCommandP1 pos (Command Down param) = descend param pos
executeCommandP1 pos (Command Up param) = ascend param pos

executeCommandP2 :: Position -> Command -> Position
executeCommandP2 pos (Command Forward param) = descendWithAim param $ forward param pos
executeCommandP2 pos (Command Down param) = adjustAim param pos
executeCommandP2 pos (Command Up param) = adjustAim (-param) pos

main :: IO ()
main = do
    contents <- getContents

    let input = map parseCommand $ map words $ lines contents
        getOutput = (\ (Position depth distance _) -> depth * distance)

    putStr "Position times depth for part 1: "
    print $ getOutput $ foldl executeCommandP1 (Position 0 0 0) input

    putStr "Position times depth for part 2: "
    print $ getOutput $ foldl executeCommandP2 (Position 0 0 0) input
