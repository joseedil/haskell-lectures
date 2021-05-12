module Main where

act :: IO (Char, Char)
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x,y)

act' :: IO (Char, Char)
act' = getChar >>= \x ->
       getChar >>
       getChar >>= \y ->
       return (x,y)

main :: IO ()
--main = do p <- act'
--          putStrLn $ show p
main = act' >>= putStrLn . show



