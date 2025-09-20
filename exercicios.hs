-- 1
salario :: Float -> Float 
salario s = (s*1.10)-(0.07 * s) 

-- 2
media :: (Float, Float, Float) -> Float 
media (a, b, c) = (((a * 2) + (b * 3) + (c * 5))/10)

mediap :: (Float, Float, Float) -> Char 
mediap (a, b, c) | media(a, b, c) > 8 = 'A' 
        | media(a, b, c) > 7 = 'B' 
        | media(a, b, c) > 6 = 'C'
        | media(a, b, c) > 5 = 'D' 
        | otherwise = 'E'

--3
tarifaFoto :: Int -> Double
tarifaFoto n
  | n == 1    = 100.0
  | n == 2    = 130.0
  | n == 3    = 150.0
  | n == 4    = 165.0
  | n == 5    = 175.0
  | n == 6    = 180.0
  | n >= 7    = 185.0
  | otherwise = 0.0

diaRetrato :: Int -> String -> Double
diaRetrato n dia | (dia == "sabado" || dia == "domingo") = (tarifaFoto n) * 1.2
diaRetrato n dia = tarifaFoto n

precoRetrato :: Int -> String -> Double
precoRetrato n dia = diaRetrato n dia

--10
data Produto = ProdutoPerecivel Int String String Bool Tipo
             | ProdutoNaoPerecivel Int String String String Tipo
             deriving Show 
-- 11
data Tipo = Unidade Int 
            | Peso Float 
            deriving Show 

--4
fatorial :: Integer -> Integer 
fatorial n | n <= 0 = 1
fatorial n = fatorial (n - 2) * n

-- 5
potencia :: Float -> Integer -> Float
potencia x 0 = 1
potencia x n | n > 0  = x * potencia x (n-1)

--6 
salarioanual :: Float -> Int -> Int -> Float
salarioanual sal anoc anoa = salarioaumentorecur sal (anoa - anoc)

salarioaumentorecur :: Float -> Int -> Float
salarioaumentorecur sal anos | anos <= 1 = sal
salarioaumentorecur sal anos = salarioaumentorecur (sal*(1 + (0.015*fromIntegral anos))) (anos-1)

-- 16
let pot_dois x | x <= 0 = 1
| otherwise = 2 * pot_dois (x-1)
in pot_dois 6
