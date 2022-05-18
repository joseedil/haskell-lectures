module Main where

import Data.List

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z [] = z
myFoldl f z (x:xs) = let z' = z `f` x
                     in myFoldl f z' xs

factorial :: Integer -> Integer
factorial n = foldl' (*) 1 [1..n]

main :: IO ()
main = putStrLn . show $ factorial 500000
