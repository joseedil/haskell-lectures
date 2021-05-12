module Main where

import System.Environment (getArgs)

fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = (fibonacci $ n - 1) + (fibonacci $ n - 2)

main :: IO ()
main = do
  n <- getArgs
  putStrLn . show . fibonacci $ read . head $ n

