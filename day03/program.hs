import Data.List (transpose, foldl')
import Data.Bits (Bits(zeroBits, setBit, complement))
import Data.Char (digitToInt)

data CommonBit = One | Zero | ZeroOne deriving (Eq, Show)

main = do
    input <- readFile "input.txt"

    let diagnosticReport = lines input
        gammaRate = evaluateGammaRate diagnosticReport
        epsilonRate = evaluateEpsilonRate diagnosticReport
        oxygenRating = evaluateOxygenRating diagnosticReport 
        co2Rating = evaluateCO2Rating diagnosticReport 

    putStrLn $ "Part 1: " ++ show (epsilonRate * gammaRate)
    putStrLn $ "Part 2: " ++ show (oxygenRating * co2Rating)

commonBitsOfReport :: [String] -> [CommonBit]
commonBitsOfReport = map (bitCountToCommonBit . countBits) . transpose

evaluateRating :: (CommonBit -> CommonBit -> Bool) -> [String] -> [CommonBit] -> Int -> Int 
evaluateRating comp possibleRatings commonBits index 
    | length possibleRatings == 1 = binStrToInt . head $ possibleRatings
    | otherwise = evaluateRating comp possibleRatings' commonBits' (index + 1)

    where possibleRatings' = filter (\rating -> comp (commonBits !! index) (charToCommonBit $ rating !! index)) possibleRatings
          commonBits' = commonBitsOfReport possibleRatings'

evaluateOxygenRating :: [String] -> Int
evaluateOxygenRating ratings = evaluateRating (\compBit bit -> bit == compBit || (compBit == ZeroOne && bit == One)) ratings commonBits 0
    where commonBits = commonBitsOfReport ratings

evaluateCO2Rating :: [String] -> Int
evaluateCO2Rating ratings = evaluateRating (\compBit bit -> (bit /= compBit && compBit /= ZeroOne) || (compBit == ZeroOne && bit == Zero)) ratings commonBits 0
    where commonBits = commonBitsOfReport ratings

evaluateRate :: (CommonBit -> Bool) -> [CommonBit] -> Int
evaluateRate _ [] = zeroBits
evaluateRate comp (first:rest) = 
    if comp first then
        setBit subRate (length rest)
    else
        subRate
    where
        subRate = evaluateRate comp rest

evaluateGammaRate :: [String] -> Int
evaluateGammaRate report = evaluateRate (\bit -> bit == One || bit == ZeroOne) commonBits
    where commonBits = commonBitsOfReport report

evaluateEpsilonRate :: [String] -> Int 
evaluateEpsilonRate report = evaluateRate (\bit -> bit == Zero || bit == ZeroOne) commonBits
    where commonBits = commonBitsOfReport report

countBits :: String -> (Int, Int)
countBits bits = (count '0' bits, count '1' bits)

bitCountToCommonBit :: (Int, Int) -> CommonBit
bitCountToCommonBit (zeros, ones)
    | zeros < ones = One
    | zeros > ones = Zero
    | otherwise = ZeroOne

charToCommonBit :: Char -> CommonBit
charToCommonBit '0' = Zero
charToCommonBit '1' = One
charToCommonBit _ = error "Unknown bit!"

count :: (Eq a) => a -> [a] -> Int
count val = length . filter (==val)

binStrToInt :: String -> Int
binStrToInt = foldl' (\acc bit -> acc * 2 + digitToInt bit) 0