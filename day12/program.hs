import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Char (isUpper)
import Data.List (partition)

type Node = String
type Path = [Node]
type Connections = Map.Map Node [Node]

main = do
    input <- readFile "input.txt"

    let connectionStrs = lines input
        connections = foldl addConnectionFromStr Map.empty connectionStrs

    putStrLn $ "Part 1: " ++ show (length . getPossiblePathsPart1 connections ["start"] $ "end")
    putStrLn $ "Part 2: " ++ show (length . getPossiblePathsPart2 connections ["start"] "end" $ False)

addConnectionFromStr :: Connections -> String -> Connections
addConnectionFromStr connections connectionStr = Map.insertWith (++) end [start]. Map.insertWith (++) start [end] $ connections
    where [start, end] = splitOn "-" connectionStr

getPossiblePathsPart1 :: Connections -> Path -> Node -> [Path]
getPossiblePathsPart1 connections path target
    | head path == target = [path]
    | otherwise = concatMap (\cave -> getPossiblePathsPart1 connections (cave:path) target) allowedCaves
    where (Just possibleCaves) = Map.lookup (head path) connections
          allowedCaves = filter (\cave -> isBigCave cave || cave `notElem` path) possibleCaves

getPossiblePathsPart2 :: Connections -> Path -> Node -> Bool -> [Path]
getPossiblePathsPart2 connections path target usedSmallTwice
    | head path == target = [path]
    | usedSmallTwice = concatMap (\cave -> getPossiblePathsPart2 connections (cave:path) target usedSmallTwice) alwaysUsable'
    | otherwise = let reusedSmallCaves = map (\cave -> getPossiblePathsPart2 connections (cave:path) target True) onlyOnceUsable'
                      withoutReusedCaves = map (\cave -> getPossiblePathsPart2 connections (cave:path) target False) alwaysUsable'
                  in concat (reusedSmallCaves ++ withoutReusedCaves)
    where (Just possibleCaves) = Map.lookup (head path) connections
          possibleCaves' = map (\cave -> (cave, cave `elem` path)) . filter (/="start") $ possibleCaves
          (alwaysUsable, onlyOnceUsable) = partition (\(cave, alreadyUsed) -> isBigCave cave || not alreadyUsed) possibleCaves'
          alwaysUsable' = map fst alwaysUsable
          onlyOnceUsable' = map fst onlyOnceUsable

isBigCave :: String -> Bool
isBigCave = isUpper . head