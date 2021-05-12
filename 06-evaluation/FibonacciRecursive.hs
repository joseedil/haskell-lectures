module Main where

fibonacci :: Integer -> Integer
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = (fibonacci $ n - 1) + (fibonacci $ n - 2)

main :: IO ()
main = putStrLn . show . fibonacci $ 40
