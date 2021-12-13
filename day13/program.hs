import Data.List.Split (splitOn)
import Data.List (isPrefixOf)

-- WHY is this code extremely slow ?
main = do
    input <- readFile "input.txt"

    let dotPositions = map ((\[x, y] -> (read x, read y)) . splitOn ",") . takeWhile (not . null) . lines $ input :: [(Int, Int)]
        maxX = maximum . map fst $ dotPositions
        maxY = maximum . map snd $ dotPositions
        dottedPaper = [[(x, y) `elem` dotPositions | x <- [0 .. maxX]] | y <- [0 .. maxY]]
        folds = map (\foldInstruction -> ('x' `elem` foldInstruction, 'y' `elem` foldInstruction)) . dropWhile (not . isPrefixOf "fold") . lines $ input
        foldedPaper = foldl applyFold dottedPaper folds

    putStrLn $ "Part 1: " ++ show (sum . map fromEnum . concat . applyFold dottedPaper $ head folds)
    putStrLn "Part 2:"
    putStrLn . unlines . map  rowToString $ foldedPaper

    where rowToString = map (\isDot -> if isDot then '#' else '.')

applyFold :: [[Bool]] -> (Bool, Bool) -> [[Bool]]
applyFold paper (foldX, foldY) = [[isDotAfterFold (x, y) | x <- [0 .. newSizeX - 1]] | y <- [0 .. newSizeY - 1]] 
    where sizeY = length paper
          sizeX = length . head $ paper
          newSizeX = if foldX then (sizeX - 1) `div` 2 else sizeX
          newSizeY = if foldY then sizeY `div` 2 else sizeY

          isDotAtPosition (x, y) = paper !! y !! x
          isDotAfterFold pos@(x, y) = isDotAtPosition pos  || (foldX && isDotAtPosition (sizeX - x - 1, y)) || (foldY && isDotAtPosition (x, sizeY - y - 1))