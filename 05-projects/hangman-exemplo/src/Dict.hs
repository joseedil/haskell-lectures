{-# LANGUAGE OverloadedStrings #-}

module Dict where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Random

type Length = Int
type Dict = T.Text
type Entry = T.Text


dictSize :: Dict -> Int
dictSize = length . T.words 

dictGetLine :: Dict -> Int -> Entry
dictGetLine dict n = (T.words dict) !! n

dictRandomEntry :: Dict -> IO Entry
dictRandomEntry dict = pure (dictGetLine dict) <*> randomRIO (0,max) 
  where max = dictSize dict - 1
  
dictOpen :: FilePath -> IO Dict
dictOpen = T.readFile  
