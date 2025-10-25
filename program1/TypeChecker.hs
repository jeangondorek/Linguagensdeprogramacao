module TypeChecker where

import Lexer

typeof :: Expr -> Maybe Ty
typeof BTrue = Just TBool
typeof BFalse = Just TBool
typeof (Num n) = Just TNum

typeof (Add e1 e2) = case (typeof e1, typeof e2) of 
                     (Just TNum, Just TNum) -> Just TNum
                     _ -> nothing

typeof (Times e1 e2) = case (typeof e1, typeof e2) of 
                     (Just TNum, Just TNum) -> Just TNum
                     _ -> nothing

typeof (And e1 e2) = case (typeof e1, typeof e2) of 
                     (Just TBool, Just TBool) -> Just TBool
                     _ -> nothing

typeof (Or e1 e2) = case (typeof e1, typeof e2) of 
                     (Just TBool, Just TBool) -> Just TBool
                     _ -> nothing
