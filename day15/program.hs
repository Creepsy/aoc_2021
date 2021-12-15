import qualified Data.List as List
import Data.Char (digitToInt)
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace (trace)

type Position = (Int, Int)

main = do
    input <- readFile "input.txt"

    let riskLevelArray = [map digitToInt row | row <- lines input]
        sizeX = length . head $ riskLevelArray
        sizeY = length riskLevelArray
        riskLevelMap = Map.fromList $ zip [(x, y) | y <- [0 .. sizeY - 1], x <- [0 .. sizeX - 1]] (concat riskLevelArray)
        enlargedRiskLevelMap = Map.fromList . concatMap (enlargedRiskLevels (sizeX, sizeY)) . Map.toList $ riskLevelMap
        mostHarmlessPath = findBestPath riskLevelMap (sizeX - 1, sizeY - 1) [([(0, 0)], 0)] (Set.singleton (0, 0))
        enlargedMostHarmlessPath = findBestPath enlargedRiskLevelMap (sizeX * 5 - 1, sizeY * 5 - 1) [([(0, 0)], 0)] (Set.singleton (0, 0))

    putStrLn $ "Part 1: " ++ show (snd mostHarmlessPath)
    putStrLn $ "Part 2: " ++ show (snd enlargedMostHarmlessPath)

    where normRiskLevel riskLevel = if riskLevel > 9 then riskLevel `mod` 9 else riskLevel 
          enlargedPositions (sizeX, sizeY) (x, y) = [(x + xOff * sizeX, y + yOff * sizeY) | xOff <- [0 .. 4], yOff <- [0 .. 4]]
          enlargedRiskLevels gridSize (pos, riskLevel) = zip (enlargedPositions gridSize pos) [normRiskLevel (riskLevel + xOff + yOff) | xOff <- [0 .. 4], yOff <- [0 .. 4]]

findBestPath :: Map.Map Position Int  -> Position -> [([Position], Int)] -> Set.Set (Int, Int) -> ([Position], Int)
findBestPath riskLevelMap end paths visited
    | (head . fst $ mostHarmless) == end = mostHarmless
    | otherwise = findBestPath riskLevelMap end paths' visited'
    where isValidPosition pos@(x, y) = pos `Map.member` riskLevelMap && Set.notMember pos visited
          mostHarmless = head paths
          headPosition = head . fst $ mostHarmless
          neighboursToVisit = let (x, y) = headPosition in filter isValidPosition [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
          visited' = Set.union visited (Set.fromList neighboursToVisit)
          newPaths = map (:fst mostHarmless) neighboursToVisit
          newPaths' = map (\path@(pos:pr) -> (path, snd mostHarmless + Map.findWithDefault 0 pos riskLevelMap)) newPaths
          paths' = foldl (flip . List.insertBy $ (compare `on` snd)) (tail paths) newPaths'