import Data.List.Split (splitOn)
import Data.List (transpose)

type BingoBoard = [[(Int, Bool)]]

main = do
    input <- readFile "input.txt"

    let seperatedInput = filter (not . null) . lines $  input
        randomNumbers = map read . splitOn "," . head $ seperatedInput :: [Int]
        unparsedBingoBoards = group 5 . tail $ seperatedInput
        bingoBoards = map parseBingoBoard unparsedBingoBoards

    putStrLn $ "Part 1: " ++ show (calculateScore $ evaluateFirstWinningBoard bingoBoards randomNumbers)
    putStrLn $ "Part 2: " ++ show (calculateScore $ evaluateLastWinningBoard bingoBoards randomNumbers)

    where calculateScore (board,lastNum) = lastNum * calculateBoardScore board
        
calculateBoardScore :: BingoBoard -> Int 
calculateBoardScore = sum . map fst . filter (not . snd) . concat

evaluateWinningOrder :: [BingoBoard] -> [Int] -> [(BingoBoard, Int)]
evaluateWinningOrder _ [] = error "No random numbers left!"
evaluateWinningOrder [] _ = []
evaluateWinningOrder boards randomNumbers = zip winningBoards (repeat . head $ randomNumbers) ++ evaluateWinningOrder remainingBoards (drop 1 randomNumbers)
    where boards' = map (markNumber $ head randomNumbers) boards 
          winningBoards = filter hasBoardWon boards'
          remainingBoards = filter (not . hasBoardWon) boards'

evaluateLastWinningBoard :: [BingoBoard] -> [Int] -> (BingoBoard, Int)
evaluateLastWinningBoard boards = last . evaluateWinningOrder boards

evaluateFirstWinningBoard :: [BingoBoard] -> [Int] -> (BingoBoard, Int)
evaluateFirstWinningBoard boards = head . evaluateWinningOrder boards
          

hasBoardWon :: BingoBoard -> Bool
hasBoardWon board = 5 `elem` markCounts
    where
        markCountsRows = map countMarked board
        markCountsCols = map countMarked . transpose $ board
        markCounts = markCountsRows ++ markCountsCols

        countMarked l = foldl (\acc (_,marked) -> acc + fromEnum marked) 0 l

markNumber :: Int -> BingoBoard -> BingoBoard
markNumber toMark board = [[(num, isMarked || toMark == num) | (num, isMarked) <- row] | row <- board]

parseBingoBoard :: [String] -> BingoBoard
parseBingoBoard unparsedBingoBoard = [[(read numStr, False) | numStr <- words row] | row <- unparsedBingoBoard] 

group :: Int -> [a] -> [[a]]
group _ [] = []
group size toGroup = take size toGroup : group size (drop size toGroup)