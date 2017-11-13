{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import Data.List (nub, sortBy)
import Data.Function
import Data.Char (isLetter, toLower)
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Map hiding (map, filter)

main :: IO ()
main = do
  [s] <- getArgs
  req <- parseRequest s
  response <- httpLBS req
  mapM_ (putStrLn . showWordCount) $ take 5 $ sortWordCount $ wordCount $ map cleanWord $ wordList response

wordList = words . (map toLower) . L8.unpack . getResponseBody

cleanWord = filter isLetter

wordCount xs = zip xs $ map (\x -> length (filter (== x) xs)) $ nub xs

showWordCount (word, count) = word ++ ": " ++ show count

sortWordCount :: Ord count => [(word, count)] -> [(word, count)]
sortWordCount = sortBy (flip compare `on` snd)
