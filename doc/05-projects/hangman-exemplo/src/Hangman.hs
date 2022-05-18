module Hangman where

import Dict

import System.IO
import qualified Data.Text as T

hangman :: IO ()
hangman = do
  dict <- dictOpen "data/dict-br-utf8.txt"
  word <- dictRandomEntry dict
  putStrLn "Tente adivinhar a palavra: "
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
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "Você acertou!"
    else do putStrLn (match word guess)
            play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '_' | x <- xs]
