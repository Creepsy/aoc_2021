import Data.List.Split (splitOn)
main = do
    input <- readFile "input.txt"

    let initialFish = map read . splitOn "," $ input :: [Int]
        initialFishSwarm = [countOccurence initialFish state | state <- [0..8]]
        finalFishCountPart1 = sum $ simulateFishSwarm initialFishSwarm 80
        finalFishCountPart2 = sum $ simulateFishSwarm initialFishSwarm 256

    putStrLn $ "Part 1: " ++ show finalFishCountPart1
    putStrLn $ "Part 2: " ++ show finalFishCountPart2

simulateFishSwarm :: [Int] -> Int -> [Int]
simulateFishSwarm fishSwarm 0 = fishSwarm
simulateFishSwarm fishSwarm days = simulateFishSwarm fishSwarm'' (days - 1)
    where
        parentFishs = head fishSwarm 
        fishSwarm' = take 9 . drop 1 . cycle $ fishSwarm
        fishSwarm'' = take 6 fishSwarm' ++ [fishSwarm' !! 6 + parentFishs] ++ drop 7 fishSwarm'

countOccurence :: (Eq a) => [a] -> a -> Int
countOccurence [] _ = 0
countOccurence (first:rest) toCount = fromEnum (first == toCount) + countOccurence rest toCount