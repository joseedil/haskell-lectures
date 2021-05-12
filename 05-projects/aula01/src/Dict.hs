{-# LANGUAGE OverloadedStrings #-}

module Dict (dictRandomEntry) where

import qualified Data.Text as T
import System.Random

type Length = Int
type Dict = T.Text
type Index = Int
type Entry = T.Text

dictSize :: Dict -> Length
dictSize dict = length . T.words $ dict

dictGetEntry :: Dict -> Index -> Entry
dictGetEntry dict i = (T.words dict) !! i

dictRandomEntry :: Dict -> IO Entry
dictRandomEntry dict =
  randomRIO (0, max) >>= return . dictGetEntry dict
  where max = dictSize dict - 1
