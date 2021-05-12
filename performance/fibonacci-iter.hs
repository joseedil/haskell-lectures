module Main where

import System.Environment

-- fibonacci :: (Integer, Integer) -> Integer -> Integer
-- fibonacci (i, j) 1 = i
-- fibonacci (i, j) 2 = j
-- fibonacci (i, j) n = (fibonacci (j, j+i) $ n - 1)

-- fibonacci' :: Integer -> Integer
-- fibonacci' n = fibonacci (1,1) n

fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

fibonacci :: Int -> Integer
fibonacci n = head . drop (n-1) $ fibs


main :: IO ()
main = do
  n <- getArgs
  putStrLn . show $ fibonacci $ read . head $ n
--  putStrLn "Hello"

