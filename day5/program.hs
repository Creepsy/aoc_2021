import Data.List.Split (splitOn)
import Data.Bool (Bool)
import Data.Function (on)
import qualified Data.Map as Map

type HydrothermalVent = ((Int, Int), (Int, Int))
type VentOccurenceMap = Map.Map (Int, Int) Int

main = do
    input <- readFile "input.txt"

    let vents = map parseHydrothermalVent . lines $ input
        straightVents = filter isStraightVent vents
        straightVentMap = foldl addVentToMap Map.empty straightVents
        ventMap = foldl addVentToMap Map.empty vents
    
    putStrLn $ "Part 1: " ++ show (countCriticalOccurences straightVentMap 2)
    putStrLn $ "Part 2: " ++ show (countCriticalOccurences ventMap 2)

countCriticalOccurences :: VentOccurenceMap -> Int -> Int
countCriticalOccurences ventMap minOcc = Map.size . Map.filter (>=minOcc) $ ventMap

addVentToMap :: VentOccurenceMap -> HydrothermalVent -> VentOccurenceMap
addVentToMap ventMap vent
    | dirX == 0 && dirY == 0 = ventMap'
    | otherwise = addVentToMap ventMap' ((dirX + (fst . fst $ vent), dirY + (snd . fst $ vent)), snd vent)
    where dirX = fromEnum ((fst . snd $ vent) `compare` (fst . fst $ vent)) - 1
          dirY = fromEnum ((snd . snd $ vent) `compare` (snd . fst $ vent)) - 1

          newVentOccur = Map.findWithDefault 0 (fst vent) ventMap + 1
          ventMap' = Map.insert (fst vent) newVentOccur ventMap

getMapSize :: [HydrothermalVent] -> (Int, Int)
getMapSize vents = (maxX + 1, maxY + 1)
    where maxX = maximum . concat $ [[fst . fst $ vent, fst . snd $ vent] | vent <- vents]
          maxY = maximum . concat $ [[snd . fst $ vent, snd . snd $ vent] | vent <- vents]

isStraightVent :: HydrothermalVent -> Bool
isStraightVent vent = uncurry ((==) `on` snd) vent || uncurry ((==) `on` fst) vent

parseHydrothermalVent :: String -> HydrothermalVent
parseHydrothermalVent ventStr = ((x1, y1), (x2, y2))
    where
        [start, end] = splitOn " -> " ventStr
        [x1, y1] = map read . splitOn "," $ start :: [Int]
        [x2, y2] = map read . splitOn "," $ end :: [Int]