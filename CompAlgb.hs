-- Utility
inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x = x - 1

neg :: Int -> Int
neg x = (-x)

-- Soma
soma :: Int -> Int -> Int
soma x y | y == 0 = x
         | y > 0 = soma (inc x) (dec y)
         | otherwise = soma (dec x) (inc y)
-- Subtracao
subt :: Int -> Int -> Int
subt x y | y == 0 = x
         | y > 0 = subt (dec x) (dec y)
         | otherwise = subt (inc x) (inc y)
-- Multiplicacao
multi :: Int -> Int -> Int
multi x y | y == 1 = x
          | y > 1 = soma x (multi x (dec y))
          | y == 0 = 0
          | y == (neg 1) = (neg x)
          | otherwise = soma (neg x) (multi x (inc y))
-- Divisao
divs :: Int -> Int -> Int
divs x y | y == 0 = error "Dividindo por zero"
         | x < y && x >= 0 = 0
         | x < y && (x+y) > 0 = 0
         | x > 0 = inc (divs (subt x y) y)
         | otherwise = dec (divs (soma x y) y)
-- Potencia
potn :: Int -> Int -> Int
potn x y | y == 0 = 1
         | y > 0 = multi x (potn x (dec y))
         | x == 1 = 1 -- unico que nao cai no caso de baixo
         | otherwise = 0 -- sera sempre menor que 1 e maior que 0, logo real
-- Logaritimo
logr :: Int -> Int -> Int
logr y x | x < 0 || y < 0 || (x == y && x == 0) = error "Base ou logaritimando invalido"
         | x == 0 = error "Infinito"
         | x < y = 0
         | otherwise = inc (logr y (divs x y))
