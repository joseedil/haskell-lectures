module Game.Hangman where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Dict


hangman :: IO ()
hangman = do putStrLn "Gerando uma palavra... "
             dict <- T.readFile "data/dict-br-utf8.txt"
             word <- dictRandomEntry dict
             putStrLn "Agora tente adivinhar a palavra: "
             play $ T.unpack word

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
