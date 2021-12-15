import Data.Char (digitToInt)
import Data.List (sortBy)
import Data.Function (on)

main = do
    input <- readFile "input.txt"

    let heightmap = [[digitToInt element | element <- row] | row <- lines input]  
        riskLevels = [calculateRiskLevel heightmap (x, y) | y <- [0..length heightmap - 1], x <- [0..length (heightmap !! y) - 1]]
        basins = [if isSmokeSink heightmap (x, y) then findBasin heightmap  [] (x, y) else [] | y <- [0..length heightmap - 1], x <- [0..length (heightmap !! y) - 1]]
        sortedBasins = sortBy (compare `on` length) basins

    putStrLn $ "Part 1: " ++ show (sum riskLevels)
    putStrLn $ "Part 2: " ++ show (product . map length . take 3 . reverse $ sortedBasins)

findBasin :: [[Int]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
findBasin heightmap visited pos@(x, y) = foldl (\acc n -> if n `elem` acc then acc else findBasin heightmap acc n) visited' biggerNeighboursWithoutBorders
    where neighbours = filter (isValidPosition heightmap) . getNeighbours $ pos
          biggerNeighbours = filter (\posN -> getHeight heightmap pos < getHeight heightmap posN) neighbours
          biggerNeighboursWithoutBorders = filter (\posN -> getHeight heightmap posN /= 9) biggerNeighbours
          visited' = pos : visited
          

isSmokeSink :: [[Int]] -> (Int, Int) -> Bool
isSmokeSink heightmap pos = length biggerNeighbours == length neighbours
    where neighbours = filter (isValidPosition heightmap) . getNeighbours $ pos
          biggerNeighbours = filter (\posN -> getHeight heightmap pos < getHeight heightmap posN) neighbours

isValidPosition :: [[Int]] -> (Int, Int) -> Bool
isValidPosition heightmap (x, y) = x >= 0 && y >= 0 && x < (length . head $ heightmap) && y < length heightmap

calculateRiskLevel :: [[Int]] -> (Int, Int) -> Int
calculateRiskLevel heightmap pos = if isSmokeSink heightmap pos then getHeight heightmap pos + 1 else 0

getHeight :: [[Int]] -> (Int, Int) -> Int
getHeight heightmap (x, y) =  heightmap !! y !! x

getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]