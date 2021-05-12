module Main where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f z (x:xs) = x `f` myFoldr f z xs

factorial :: Integer -> Integer
factorial n = myFoldr (*) 1 [1..n]

main :: IO ()
main = putStrLn . show $ factorial 500000
