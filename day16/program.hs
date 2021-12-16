import Data.Char (digitToInt)
import qualified Data.List as List
import Data.Maybe (fromJust)

data Packet = LiteralPacket Int Int Int | OperatorPacket Int Int [Packet] deriving (Show)

main = do
    input <- readFile "input.txt"

    let packet = concat . lines $ input
        binaryData = concatMap (intToBinStr 4 . digitToInt) packet
        (transmissionPacket, unparsed) = parsePacket binaryData

    putStrLn $ "Part 1: " ++ show (sum . packetMap packetVersion $ transmissionPacket)
    putStrLn $ "Part 2: " ++ show (evaluatePacket transmissionPacket)

packetVersion :: Packet -> Int
packetVersion (LiteralPacket version _ _) = version
packetVersion (OperatorPacket version _ _) = version

packetTypeID :: Packet -> Int 
packetTypeID (LiteralPacket _ id _) = id
packetTypeID (OperatorPacket _ id _) = id

packetMap :: (Packet -> a) -> Packet -> [a]
packetMap func packet@(OperatorPacket _ _ subPackets) = func packet : concatMap (packetMap func) subPackets
packetMap func packet = [func packet]

intToBinStr :: Int -> Int -> String
intToBinStr strSize toConvert = ['0' | _ <- [1 .. (strSize - length binStr)]] ++ binStr
    where convertReversed 0 = []
          convertReversed num = (if even num then '0' else '1') : convertReversed (num `div` 2)
          binStr = reverse . convertReversed $ toConvert

binStrToInt :: String -> Int
binStrToInt = foldl (\acc d -> 2 * acc + digitToInt d) 0

group :: Int -> [a] -> [[a]]
group _ [] = []
group groupSize toGroup = take groupSize toGroup : group groupSize (drop groupSize toGroup)

parseLiteralValue :: String -> (Int, String)
parseLiteralValue toParse = (binStrToInt . concatMap tail . take literalGroups $ bitGroups, concat . drop literalGroups $ bitGroups)
    where bitGroups = group 5 toParse
          literalGroups = 1 + (fromJust . List.findIndex ((== '0') . head) $ bitGroups)

packetIter :: String -> [([Packet], String)]
packetIter toParse = iterate parseNext ([], toParse)
    where parseNext (packets, remaining) = let (packet, remaining') = parsePacket remaining in (packet:packets, remaining')

parsePackets :: Int -> String -> ([Packet], String)
parsePackets 0 toParse = head . dropWhile (\(_, remaining) -> (length toParse' - length remaining) < subPacketLength) . packetIter $ toParse'
    where (subPacketLengthStr, toParse') = List.splitAt 15 toParse
          subPacketLength = binStrToInt subPacketLengthStr

parsePackets 1 toParse = packetIter toParse' !! subPacketCount
    where (subPacketCountStr, toParse') = List.splitAt 11 toParse
          subPacketCount = binStrToInt subPacketCountStr
          
parsePackets _ _ = error "Invalid length type ID!"

parsePacket :: String -> (Packet, String)
parsePacket toParse 
    | typeID == 4 = let (literal, remaining) = parseLiteralValue toParse'' in (LiteralPacket packetVersion typeID literal, remaining)
    | otherwise = let (subPackets, remaining) = parsePackets (binStrToInt lengthTypeIDStr) toParse''' in (OperatorPacket packetVersion typeID (reverse subPackets), remaining)

    where 
          (packetVersionStr, toParse') = List.splitAt 3 toParse
          (typeIDStr, toParse'') = List.splitAt 3 toParse'
          packetVersion = binStrToInt packetVersionStr
          typeID = binStrToInt typeIDStr

          (lengthTypeIDStr, toParse''') = List.splitAt 1 toParse''
          
          subPacketCount = binStrToInt . take 11  $ toParse'''
          
evaluatePacket :: Packet -> Int
evaluatePacket (LiteralPacket _ _ literal) = literal
evaluatePacket (OperatorPacket _ 0 subPackets) = sum . map evaluatePacket $ subPackets
evaluatePacket (OperatorPacket _ 1 subPackets) = product . map evaluatePacket $ subPackets
evaluatePacket (OperatorPacket _ 2 subPackets) = minimum . map evaluatePacket $ subPackets
evaluatePacket (OperatorPacket _ 3 subPackets) = maximum . map evaluatePacket $ subPackets
evaluatePacket (OperatorPacket _ 5 (f:s:_)) = fromEnum $ evaluatePacket f > evaluatePacket s
evaluatePacket (OperatorPacket _ 6 (f:s:_)) = fromEnum $ evaluatePacket f < evaluatePacket s
evaluatePacket (OperatorPacket _ 7 (f:s:_)) = fromEnum $ evaluatePacket f == evaluatePacket s
evaluatePacket (OperatorPacket _ _ subPackets) = error "Invalid packet operator!"