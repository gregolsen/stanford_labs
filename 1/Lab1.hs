import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Monad as Monad
import System.Environment
import System.IO

cutWord :: String -> String
cutWord = reverse . dropWhile (`elem` "!.,:;-')(=") . reverse

splitLine :: String -> [String]
splitLine = filter (\x -> length x > 0) . map (map Char.toLower . cutWord) . words

maxWordLen :: [String] -> Int
maxWordLen = maximum . map length

type WordStat = (String, Int)

toDictList :: [String] -> [WordStat]
toDictList = map (\x -> (x, 1))

buildDict :: [WordStat] -> Map.Map String Int
buildDict = Map.fromListWith (+)

process :: [String] -> [WordStat]
process = List.sortBy (\(_, v1) (_, v2) -> v2 `compare` v1) . Map.toList . buildDict . toDictList

calcBarLen :: Int -> Int -> Int
calcBarLen value maxLen = round(fromIntegral(80 * value) / fromIntegral maxLen)

printWordBar :: Int -> Int -> WordStat -> IO ()
printWordBar maxWord maxLen (word, value) =
  let spaces = take (1 + maxWord - length word) $ cycle " "
      bar = take (fromIntegral(calcBarLen value maxLen)) $ cycle "#"
  in putStrLn $ word ++ spaces ++ bar

main = do
  args <- getArgs
  text <- if null args
    then getContents
    else fmap concat . Monad.forM args $ readFile
  let 
    words   = splitLine text
    maxWord = maxWordLen words
    result  = process words
    maxLen  = snd $ head result
  mapM_ (printWordBar maxWord maxLen) $ filter (\(word, value) -> calcBarLen value maxLen > 0) result
