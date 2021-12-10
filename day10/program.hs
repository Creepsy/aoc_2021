import Data.Either (isRight, isLeft, fromLeft)
import Data.List (sort)

data TokenType = Open | Close deriving (Eq)
type Stack = [Char]
type StackError = Char

main = do
    input <- readFile "input.txt"

    let lineChunks = lines input
        processedLines = map (processTokens []) lineChunks
        corruptedLines = filter isRight processedLines
        completions = sort [calculateStackScore $ fromLeft [] line | line <- processedLines, isLeft line]

    putStrLn $ "Part 1: " ++ show (sum . map (\(Right t) -> getErrorTokenScore t) $ corruptedLines)
    putStrLn $ "Part 2: " ++ show (head . drop ((length completions - 1) `div` 2) $ completions)

calculateStackScore :: Stack -> Int
calculateStackScore stack = foldl (\acc t -> acc * 5 + getCompletionTokenScore t) 0 reversedStack
    where reversedStack = map reverseToken stack

processTokens :: Stack -> String -> Either Stack StackError
processTokens stack "" = Left stack
processTokens stack (token : remaining)
    | getTokenType token == Open = processTokens (token:stack) remaining
    | matchesTop (reverseToken token) = processTokens (tail stack) remaining
    | otherwise = Right token
    where matchesTop token = not (null stack) && (head stack == token)

getCompletionTokenScore :: Char -> Int
getCompletionTokenScore ')' = 1
getCompletionTokenScore ']' = 2  
getCompletionTokenScore '}' = 3
getCompletionTokenScore '>' = 4
getCompletionTokenScore token = error $ "Unrecognized token " ++ show token ++ "!"

getErrorTokenScore :: Char -> Int
getErrorTokenScore ')' = 3
getErrorTokenScore ']' = 57  
getErrorTokenScore '}' = 1197
getErrorTokenScore '>' = 25137
getErrorTokenScore token = error $ "Unrecognized token " ++ show token ++ "!"


getTokenType :: Char -> TokenType
getTokenType token
    | token `elem` ['{', '[', '(', '<'] = Open
    | token `elem` ['}', ']', ')', '>'] = Close
    | otherwise = error $ "Unrecognized token " ++ show token ++ "!"

reverseToken :: Char -> Char
reverseToken '{' = '}'
reverseToken '}' = '{'
reverseToken '<' = '>'
reverseToken '>' = '<'
reverseToken '(' = ')'
reverseToken ')' = '('
reverseToken '[' = ']'
reverseToken ']' = '['
reverseToken token = error $ "Unrecognized token " ++ show token ++ "!"