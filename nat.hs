data Nat = Zero | Suc Nat deriving (Show, Eq)

um   = Suc Zero
dois = Suc um
tres = Suc dois
quatro = Suc tres

nat2integer :: Nat -> Integer
nat2integer Zero     = 0
nat2integer (Suc n)  = 1 + nat2integer n

integer2nat :: Integer -> Nat
integer2nat 0 = Zero
integer2nat n
  | n > 0     = Suc (integer2nat (n-1))


natAdd :: Nat -> Nat -> Nat
natAdd Zero n = n
natAdd (Suc m) n  = Suc (natAdd m n)

natSub :: Nat -> Nat -> Nat
natSub n Zero         = n
natSub Zero _         = Zero
natSub (Suc m) (Suc n) = natSub m n

natMul :: Nat -> Nat -> Nat
natMul Zero _     = Zero
natMul (Suc m) n  = natAdd n (natMul m n)
