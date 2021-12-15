type Position = (Int, Int)
type AdvancedPosition = (Position, Int)
type Command = (String, Int)

main = do
    input <- readFile "input.txt"

    let parsedInput = map ((\[dir, amount] -> (dir, read amount :: Int)) . words) . lines $ input
        finalPositionPart1 = foldl applyCommandPart1 (0, 0) parsedInput
        finalPositionPart2 = foldl applyCommandPart2 ((0, 0), 0) parsedInput

    putStrLn $ "Part 1: " ++ show (uncurry (*) finalPositionPart1)
    putStrLn $ "Part 1: " ++ show (uncurry (*) . fst $ finalPositionPart2)

applyCommandPart1 :: Position -> Command -> Position
applyCommandPart1 (x, y) ("forward", amount) = (x + amount, y)
applyCommandPart1 (x, y) ("down", amount) = (x, y + amount)
applyCommandPart1 (x, y) ("up", amount) = (x, y - amount)
applyCommandPart1 _ _ = error "Invalid command!"

applyCommandPart2 :: AdvancedPosition -> Command -> AdvancedPosition
applyCommandPart2 ((x, y), aim) ("forward", amount) = ((x + amount, y + aim * amount), aim)
applyCommandPart2 (pos, aim) ("down", amount) = (pos, aim + amount)
applyCommandPart2 (pos, aim) ("up", amount) = (pos, aim - amount)
applyCommandPart2 _ _ = error "Invalid command!"