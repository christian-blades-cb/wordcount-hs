{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment

main :: IO ()
main = do
  [s] <- getArgs
  f <- readFile s
  mapM_ putStrLn $ words f

