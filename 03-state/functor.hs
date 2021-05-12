import Control.Monad

newtype ST s a = ST {app :: s -> (a,s)}

instance Functor (ST s) where
  -- fmap :: (a -> b) -> ST s a -> ST s b
  fmap f st = ST $ \s ->
                     let (a,s') = app st s
                     in  (f a, s')

instance Applicative (ST s) where
  -- pure :: a -> ST s a
  pure x = ST $ \s -> (x,s)

  -- (<*>) :: ST s (a -> b) -> ST s a -> ST s b
  (ST f) <*> (ST x) = ST $ \s ->
                             let (fa, s')  = f s
                                 (xa, s'') = x s'
                             in (fa xa, s'')

instance Monad (ST s) where
  --(>>=) :: ST s a -> (a -> ST s a) -> ST s b
  st >>= f = ST $ \s ->
                    let (x,s') = app st s
                    in  app (f x) s


data S = S0 | S1 | S2 | S3
  deriving (Show, Eq)

next :: S -> S
next S0 = S1
next S1 = S2
next S2 = S3
next S3 = S0

prev :: S -> S
prev S0 = S3
prev S1 = S0
prev S2 = S1
prev S3 = S2

out :: S -> Char
out S0 = 'A'
out S1 = 'B'
out S2 = 'C'
out S3 = 'D'


counter :: Char -> ST S Char
counter c = ST $ \s -> (out . ns $ s, ns s)
  where ns s
          | c == 'f'  = next s
          | c == 'r'  = prev s
          | otherwise = s


--main :: IO
fsm s = do
  c <- getChar
  let (out, ns) = app (counter c) s
  putChar out
  putChar '\n'
  fsm ns

fsm' s = do
  stream <- getLine
  let (out, ns) = app (mapM counter stream) s
  putStrLn out
  fsm' ns

main = do
  let initialState = S0
  putStrLn $ " " <> [out initialState]
  fsm' initialState
