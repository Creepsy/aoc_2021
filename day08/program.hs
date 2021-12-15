import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List ((\\))
import Data.Function (on)

main = do
    input <- readFile "input.txt"

    let measurementStrs = map (splitOn " | ") . lines $ input
        measurements = map (\[signals,output] -> (words signals, words output)) measurementStrs 
        measurementOutputs = map snd measurements

    putStrLn $ "Part 1: " ++ show (countUniqueNumbers . concat $ measurementOutputs)
    putStrLn $ "Part 2: " ++ show (sum . map getOutputNumber $ measurements)

countUniqueNumbers :: [String] -> Int
countUniqueNumbers = length . filter isUniqueNumber 
    where isUniqueNumber n = length n `elem` [2, 3, 4, 7]

getOutputNumber :: ([String], [String]) -> Int
getOutputNumber (signals, output) = foldl (\acc digit -> 10 * acc + digit) 0 . map (decodeNumber uniqueNumbers) $ output
    where uniqueNumbers = getUniqueNumbers signals

getUniqueNumbers :: [String] -> [String]
getUniqueNumbers signals = let [one, seven, four, _, _, _, _, _, _, eight] = List.sortBy (compare `on` length) signals in [one, four, seven, eight]

difference :: String -> String -> Int
difference number toCompare = length $ number \\ toCompare

decodeNumber :: [String] -> String -> Int
decodeNumber uniqueNumbers number
    | numberLen == 2 = 1
    | numberLen == 3 = 7
    | numberLen == 4 = 4
    | numberLen == 7 = 8
    | numberLen == 5 && difference (head uniqueNumbers) number == 0 = 3
    | numberLen == 5 && difference (uniqueNumbers !! 1) number == 2 = 2
    | numberLen == 5 = 5
    | numberLen == 6 && difference (uniqueNumbers !! 1) number == 0 = 9
    | numberLen == 6 && difference (head uniqueNumbers) number == 0 = 0
    | numberLen == 6 = 6
    | otherwise = error "Invalid number!"

    where numberLen = length number