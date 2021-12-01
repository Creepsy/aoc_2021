main = do
    input <- readFile "input.txt"

    let measurements = map read . lines $ input :: [Int]

    putStrLn $ "Part 1: " ++ show (countHeightIncrements measurements)
    putStrLn $ "Part 2: " ++ show (countThreeMeasurementIncrements measurements)

countHeightIncrements :: [Int] -> Int
countHeightIncrements [] = 0
countHeightIncrements (first:rest)
    | null rest = 0 
    | otherwise  = countHeightIncrements rest + fromEnum (first < head rest)

countThreeMeasurementIncrements :: [Int] -> Int 
countThreeMeasurementIncrements measurements
    | length measurements < 4 = 0
    | otherwise = 
        let 
            firstGroup = take 3 measurements
            secondGroup = take 3 . drop 1 $ measurements
        in 
            countThreeMeasurementIncrements (drop 1 measurements) + fromEnum (sum firstGroup < sum secondGroup)
