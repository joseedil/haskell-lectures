module Main where

import Data.Char (toUpper)
import Control.Applicative (liftA)

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n'
                then return []
                else do xs <- getLine'
                        return (x:xs)

main :: IO ()
--main = getLine' >>= return . upperCase >>= putStrLn
main = liftA upperCase getLine' >>= putStrLn

upperCase :: String -> String
upperCase = fmap toUpper
