module Main where

import System.IO

main = hangman

hangman :: IO ()
hangman = do putStrLn "Pense em uma palavra: "
             word <- sgetLine
             putStrLn "Agora tente adivinhar a palavra: "
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n'
                then do putChar x
                        return ""
                else do putChar '*'
                        xs <- sgetLine
                        return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x


play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word
                 then putStrLn "Você acertou!"
                 else do putStrLn (match word guess)
                         play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '_' | x <- xs]
