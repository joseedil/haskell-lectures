module Aula where


primes = sieve ([2..] :: [Integer])
sieve (p:xs) = p : sieve [x | x<- xs, mod x p /= 0]



data MyList a = Nil 
              | Cons a (MyList a)
  deriving (Show)

instance Functor MyList where
  fmap f (Nil) = Nil
  fmap f (Cons x list) = Cons (f x) (fmap f list)

fromList :: [a] -> MyList a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: MyList a -> [a]
toList Nil = []
toList (Cons x list) = x:(toList list)


myHead :: MyList a -> a
myHead (Cons x _) = x



data MyList' a = Nil' 
               | Cons' a !(MyList' a)
  deriving (Show)

instance Functor MyList' where
  fmap f (Nil') = Nil'
  fmap f (Cons' x list) = Cons' (f x) (fmap f list)

fromList' :: [a] -> MyList' a
fromList' []     = Nil'
fromList' (x:xs) = Cons' x (fromList' xs)

toList' :: MyList' a -> [a]
toList' Nil' = []
toList' (Cons' x list) = x:(toList' list)

myHead' :: MyList' a -> a
myHead' (Cons' x _) = x
