import Data.Char (digitToInt)
import Debug.Trace (trace)

type OctopusGrid = [[Int]]

main = do
    input <- readFile "input.txt"

    let  octopuses = [[digitToInt $ row !! x | x <- [0..9]] | row <- lines input]

    putStrLn $ "Part 1: " ++ show (snd . simulateSteps octopuses $ 100)
    putStrLn $ "Part 2: " ++ show (getSimultaneousFlashingStep octopuses 0)

getSimultaneousFlashingStep :: OctopusGrid -> Int -> Int
getSimultaneousFlashingStep octopuses step
    | flashes == 100 = step + 1
    | otherwise = getSimultaneousFlashingStep octopuses' (step + 1)
    where 
        (octopuses', flashes) = simulateStep octopuses

simulateSteps :: OctopusGrid -> Int -> (OctopusGrid, Int)
simulateSteps octopuses 0 = (octopuses, 0)
simulateSteps octopuses steps = (octopuses'', flashes + flashes')
    where (octopuses', flashes) = simulateStep octopuses
          (octopuses'', flashes') = simulateSteps octopuses' (steps - 1)


simulateStep :: OctopusGrid -> (OctopusGrid, Int)
simulateStep octopuses = (octopuses'', flashes)
    where octopuses' = map (map (+1)) octopuses
          octopuses'' = updateFlashingOctopuses octopuses'
          flashes = sum . map (foldl (\acc status -> if status == 0 then acc + 1 else acc) 0) $ octopuses''

updateFlashingOctopuses :: OctopusGrid -> OctopusGrid
updateFlashingOctopuses octopuses
    | null flashingOctopuses = octopuses
    | otherwise = updateFlashingOctopuses octopuses''
    where octopusesWithPosition = zipWith zip octopuses [[(xPos, yPos) | xPos <- [0..9]] | yPos <- [0..9]]
          flashingOctopuses = map snd . concatMap (filter ((>9) . fst)) $ octopusesWithPosition
          octopuses' = map (map (\status -> if status > 9 then 0 else status)) octopuses
          octopuses'' = foldl increaseValidNeighbours octopuses' flashingOctopuses       

increaseValidNeighbours :: OctopusGrid -> (Int, Int) -> OctopusGrid
increaseValidNeighbours octopuses (x, y) = [[
    if (xPos, yPos) `elem` validNeighbours && (octopuses !! yPos !! xPos) /= 0 then
        (octopuses !! yPos !! xPos) + 1 
    else
        octopuses !! yPos !! xPos 
    | xPos <- [0..9]] | yPos <- [0..9]]
    where neighbours = [(x + xOff, y + yOff) | xOff <- [-1..1], yOff <- [-1..1], xOff /= 0 || yOff /= 0]
          validNeighbours = filter (\(x, y) -> x >= 0 && y >= 0 && x < 10 && y < 10) neighbours
