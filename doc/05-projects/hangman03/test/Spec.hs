

import Hangman

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.Monad
import Data.List

-- Parte 1
main :: IO ()
main = hspec $ do
  describe "match" $ do
    it "match \"edil\" \"e\" should be \"e___\"" $ 
      match "edil" "e" `shouldBe` "e___"
    modifyMaxSuccess (const 1000) $ it "lengths" $
      property $ \s1 s2 -> (length $ match s1 s2) `shouldBe` (length s1)
    context "when given empty list to match" $ do
      it "match \"edil\" \"\" should be \"____\"" $
        pending
      it "match \"\" \"e\" should be \"\"" $ 
        pendingWith "TODO: implementar teste"


-- -- Parte 2
-- prop_reverse :: [Int] -> Bool
-- prop_reverse xs = reverse (reverse xs) == xs

-- prop_match :: String -> String -> Bool
-- prop_match s1 s2 = (length $ match s1 s2) == (length s1)

-- main :: IO ()
-- main = do
--   quickCheck prop_reverse
--   quickCheck prop_match


-- -- combinadores básicos
-- -- apply to generate or sample
-- ex01 = elements [1,2,3]
-- ex02 = choose ('a', 'z')

-- ex03 = generate $ return 1
-- ex04 = replicateM 10 $ generate ex02

-- ex05 = generate (arbitrary :: Gen [(Int, Bool)])

-- -- Gen a é Applicative!
-- data Tripla = Tripla { x :: Double
--                      , y :: Double
--                      , name :: String}
--   deriving (Show)

-- ex06 = Tripla <$> arbitrary <*> arbitrary <*> arbitrary

-- -- controlar a frequencia dos elementos gerados
-- ex07 = frequency [ (1, return '_')
--                  , (5, return '*')]

-- -- controlando o tamanho da estrutura e das amostras
-- flexList :: Arbitrary a => Gen [a]
-- flexList = sized $ \n ->
--   frequency [ (1, return [])
--             , (n, (:) <$> arbitrary <*> flexList) ]

-- ex08 = generate $ resize 5 (flexList :: Gen [Int])
-- ex09 = generate $ resize 100 (flexList :: Gen [Int])

-- ex10 = generate $ scale (*33) (flexList :: Gen [Int])


-- -- especificando propriedades
-- prop_associativeAdd :: Int -> Int -> Int -> Bool
-- prop_associativeAdd x y z = (x + y) + z == x + (y + z)

-- prop_associativeAdd' :: Double -> Double -> Double -> Bool
-- prop_associativeAdd' x y z = (x + y) + z == x + (y + z)


-- -- Parte 3
-- qsort :: Ord a => [a] -> [a]
-- qsort []     = []
-- qsort (x:xs) = qsort lhs <> [x] <> rhs
--   where
--     lhs = [e | e <- xs, e < x]
--     rhs = [e | e <- xs, e > x]

-- prop_idempotencia :: Ord a => [a] -> Bool
-- prop_idempotencia xs = qsort (qsort xs) == qsort xs

-- prop_length :: Ord a => [a] -> Bool
-- prop_length xs = length (qsort xs) == length xs

-- prop_minimum :: Ord a => [a] -> Bool
-- prop_minimum xs = head (qsort xs) == minimum xs

-- prop_model :: Ord a => [a] -> Bool
-- prop_model xs = qsort xs == sort xs 

-- main :: IO ()
-- main = hspec $ do
--   describe "quicksort" $ do
--     it "idempotencia :: [Int]" $
--       property $ (prop_idempotencia :: [Int] -> Bool) 
--     it "length :: [Int]" $
--       property (prop_length :: [Int] -> Bool)
--     it "minimum :: [Int]" $
--       property (prop_minimum :: [Int] -> Bool)
--     it "reference model :: [Int]" $
--       property (prop_model :: [Int] -> Bool)
