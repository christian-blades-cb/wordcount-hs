{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import Data.List (nub, sortBy)
import Data.Function
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TIO
import Data.Char (isLetter)

main :: IO ()
main = do
  [s] <- getArgs
  f <- TIO.readFile s
  let wList = wordList f
  mapM_ (Prelude.putStrLn . showWordCount) $ Prelude.take 5 $ sortWordCount $ wordCount $ wordList f

wordList :: Text -> [Text]
wordList f = Prelude.map (T.filter isLetter) $ T.words $ toLower f

wordCount :: [Text] -> [(Text, Int)]
wordCount xs = Prelude.zip xs $ Prelude.map (\x -> Prelude.length (Prelude.filter (== x) xs)) $ nub xs

showWordCount :: (Text, Int) -> String
showWordCount (word, count) = unpack word ++ ": " ++ show count

sortWordCount = sortBy (flip compare `on` snd)
