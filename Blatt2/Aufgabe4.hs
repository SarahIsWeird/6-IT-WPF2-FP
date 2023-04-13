data Nat
  = Zero
  | Succ Nat
  deriving (Show)

plus :: Nat -> Nat -> Nat
plus x y =
  case y of
    Zero -> x
    Succ n -> plus (Succ x) n

mult :: Nat -> Nat -> Nat
mult x y =
  case y of
    Zero -> Zero
    Succ Zero -> x
    Succ n -> plus (mult x n) x

natToInt :: Nat -> Int
natToInt n =
  case n of
    Zero -> 0
    Succ n -> 1 + natToInt n

intToNat :: Int -> Nat
intToNat i =
  case i of
    0 -> Zero
    n -> Succ . intToNat $ n - 1

main = do
  print $ intToNat 5
  print $ natToInt (intToNat 5)
  print $ natToInt (plus (intToNat 5) (intToNat 10))
  print $ natToInt (mult (intToNat 5) (intToNat 10))
