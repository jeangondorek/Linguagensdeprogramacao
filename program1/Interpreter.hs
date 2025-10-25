module Interpreter where

import Lexer
import Parser

step :: Expr -> Expr
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = Add (Num n1) (step e2)
step (Add e1 e2) = Add (step e1) e2

step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = Times (Num n1) (step e2)
step (Times e1 e2) = Times (step e1) e2

step (And (BFalse e2)) = BFalse
step (And (BTrue e2)) = e2
step (And e1 e2) = And (step e1) e2

step (Or (BTrue e2)) = BTrue
step (Or (BFalse e2)) = e2
step (Or e1 e2) = Or (step e1) e2





