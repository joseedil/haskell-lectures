module Main where

myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' f z [] = z
myFoldl' f z (x:xs) = let z' = z `f` x
                      in seq z' $ myFoldl' f z' xs

factorial :: Integer -> Integer
factorial n = myFoldl' (*) 1 [1..n]

main :: IO ()
main = putStrLn . show $ factorial 500000
