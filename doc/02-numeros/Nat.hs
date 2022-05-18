module Nat where

data Nat = Zero | Succ Nat
--  deriving (Show)

instance Eq Nat where
  Zero == Zero     = True
  Zero == Succ n   = False
  Succ m == Zero   = False
  Succ m == Succ n = (m == n)

instance Ord Nat where
  Zero < Zero     = False
  Zero < Succ n   = True
  Succ n < Zero   = False
  Succ m < Succ n = (m < n)

instance Show Nat where
  show = showNat

showNat :: Nat -> String
showNat = show . nat2int
  where
    nat2int :: Nat -> Int
    nat2int Zero = 0
    nat2int (Succ k) = 1 + nat2int k

readNat :: Integer -> Nat
readNat 0 = Zero
readNat n = Succ (readNat $ n-1)

addNat :: Nat -> Nat -> Nat
--addNat m Zero     = m                       -- m + 0 = m
--addNat m (Succ n) = Succ (addNat m n)       -- m + (n+1) = (m+n) + 1
addNat = foldn Succ

multNat :: Nat -> Nat -> Nat
--multNat m Zero   = Zero                     -- m * 0 = 0
--multNat m (Succ n) = addNat (multNat m n) m   -- m * (n+1) = (m*n) + m
multNat m = foldn (addNat m) Zero

expNat :: Nat -> Nat -> Nat
--expNat m Zero     = Succ Zero               -- m^0 = 1
--expNat m (Succ n) = multNat (expNat m n) m  -- m^(n+1) = m * m^n
expNat m = foldn (multNat m) (Succ Zero)

subNat :: Nat -> Nat -> Nat
subNat m Zero = m
subNat (Succ k) (Succ n) = subNat k n

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Zero     = c
foldn h c (Succ n) = h (foldn h c n)

