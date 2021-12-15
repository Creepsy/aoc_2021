import Data.List.Split (splitOn)
main = do
    input <- readFile "input.txt"

    let crabPositions = map read . splitOn "," $ input :: [Int]
        minPosition = minimum crabPositions
        maxPosition = maximum crabPositions
        fuelCostsPart1 = map (fuelCostToPositionPart1 crabPositions) [minPosition .. maxPosition]
        fuelCostsPart2 = map (fuelCostToPositionPart2 crabPositions) [minPosition .. maxPosition]

    putStrLn $ "Part 1: " ++ show (minimum fuelCostsPart1)
    putStrLn $ "Part 2: " ++ show (minimum fuelCostsPart2)

fuelCostToPositionPart1 :: [Int] -> Int -> Int
fuelCostToPositionPart1 crabPositions position = sum . map (abs . subtract position) $ crabPositions 

fuelCostToPositionPart2 :: [Int] -> Int -> Int
fuelCostToPositionPart2 crabPositions position = sum . map (sumOfFirstNNumbers . abs . subtract position) $ crabPositions 
    where sumOfFirstNNumbers n = n * (n + 1) `div` 2