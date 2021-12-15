import qualified Data.Map as Map
import Data.List.Split (splitOn)
import qualified Data.List as List

main = do
    input <- readFile "input.txt"

    let (polymerTemplate : insertionRuleStrs) = filter (not . null) . lines $ input
        insertionRules = Map.fromList . map parseInsertionRule $ insertionRuleStrs
        polymerGroups = zipWith (\f s -> [f, s]) polymerTemplate (take (length polymerTemplate - 1) . drop 1 . cycle $ polymerTemplate)
        reducedGroups = Map.fromList . map (\g -> (head g, length g)) . List.group . List.sort $ polymerGroups

    putStrLn $ "Part 1: " ++ show (calculatePolymerScore polymerTemplate . applyRules reducedGroups insertionRules $ 10)
    putStrLn $ "Part 2: " ++ show (calculatePolymerScore polymerTemplate . applyRules reducedGroups insertionRules $ 40)

parseInsertionRule :: String -> (String, Char)
parseInsertionRule insertionRuleStr = (input, head output)
    where [input, output] = splitOn " -> " insertionRuleStr

applyRules :: Map.Map String Int -> Map.Map String Char -> Int -> Map.Map String Int
applyRules polymerGroups _ 0 = polymerGroups
applyRules polymerGroups rules iterations = applyRules polymerGroups' rules (iterations - 1)
    where applyRulesToGroup group@[f, s] = let (Just out) = Map.lookup group rules in [[f, out], [out, s]]
          insertGroup gMap count g = Map.insertWith (+) g count gMap
          insertGroups count = foldl (`insertGroup` count)
          polymerGroups' = foldl (\acc (g, count) -> insertGroups count acc (applyRulesToGroup g)) Map.empty (Map.toList polymerGroups) :: Map.Map String Int

calculatePolymerScore :: String -> Map.Map String Int -> Int
calculatePolymerScore polymerTemplate polymerGroups = mostCommon - leastCommon
    where insertGroupElements eMap (elements, count) = foldl (\acc e -> Map.insertWith (+) e count acc) eMap elements
          polymerElements = Map.map (`div`2) . foldl insertGroupElements Map.empty $ Map.toList polymerGroups
          polymerElements' = insertGroupElements polymerElements ([head polymerTemplate, last polymerTemplate], 1)
          elementCountList = map snd (Map.toList polymerElements')
          mostCommon = maximum elementCountList
          leastCommon = minimum elementCountList