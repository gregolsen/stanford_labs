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

toDictList :: [String] -> [(String, Int)]
toDictList = map (\x -> (x, 1))

buildDict :: [(String, Int)] -> Map.Map String Int
buildDict = Map.fromListWith (+)

process :: [String] -> [(String, Int)]
process = List.sortBy (\(_, v1) (_, v2) -> v2 `compare` v1) . Map.toList . buildDict . toDictList

printWordBar :: Int -> (String, Int) -> IO ()
printWordBar maxWord (word, value) =
  let spaces = take (1 + maxWord - length word) $ cycle " "
      bar = take value $ cycle "#"
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
  mapM_ (printWordBar maxWord) result
