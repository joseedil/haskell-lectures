module Main where

import System.Environment

fibonacci :: (Integer, Integer) -> Integer -> Integer
fibonacci (i, j) 1 = i
fibonacci (i, j) 2 = j
fibonacci (i, j) n = (fibonacci (j, j+i) $ n - 1)

main :: IO ()
main = do
  n <- getArgs
  putStrLn . show $ fibonacci (1,1) $ read . head $ n
