module Lexer where
import Data.Char (isSpace, isDigit, isAlpha)

data Token = TokenNum Int
           | TokenPlus
           | TokenTrue
           | TokenFalse
           | TokenAnd
           | TokenOr
           | TokenTimes
           | TokenLParen
           | TokenRParen
           deriving Show

Data Expr = Num Int
          | BTrue
          | BFalse
          | Add Expr Expr
          | Times Expr Expr 
          | And Expr Expr 
          | Or Expr Expr
          | Paren Expr  
          deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs)
             | isAlpha c = lexKw (c:cs)
lexer _ = error "Lexical error"

lexNum cs = case span isDigit cs of
                (num, rest) -> TokenNum (read num) : lexer rest


lexKw cs = case span isAlpha cs of
            ("true", rest) -> TokenTrue : lexer rest
            ("false", rest) -> TokenFalse : lexer rest

                
