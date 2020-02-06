-- Bibliotecas
import Data.Char -- para usar toLower/toUpper

-- 2)
nOr :: Bool -> Bool -> Bool
nOr x y = ( x == y ) && ( x == False )
-- 3)
tresDiferentes :: Int -> Int -> Int -> Bool
tresDiferentes x y z = ( x /= y ) && ( y /= z ) && ( z /= x )
-- 4)
areaT :: Float -> Float -> Float
areaT b h = b * h / 2
-- 5)
areaC :: Float -> Float
areaC r = r^2 * pi
-- 6)
converte :: Float -> Float
converte f = ( 5/9 )*( f - 32 )
-- 7)
paraMinuscula :: Char -> Char
paraMinuscula c = toLower c
-- 8)
paraMaiuscula :: Char -> Char
paraMaiuscula c = toUpper c
-- 9)
media :: Float -> Float -> Float -> Float
media x y z = ( x + y + z )/3
-- 10)
posicao :: Float -> Float -> String
posicao x y = "( " ++ show x ++ " , " ++ show y ++ " )"
-- 11)
unidades :: Int -> Int
unidades x = mod x 10
-- 12)
dezenas :: Int -> Int
dezenas x = (mod x 100 - unidades x) `div` 10
-- 13)
centenas :: Int -> Int
centenas x = (mod x 1000 - dezenas x * 10 - unidades x) `div` 100
-- 14)
somaAlgarismos :: Int -> Int
somaAlgarismos x = unidades x + dezenas x + centenas x 
-- 15)
tipoTriangulo :: Int -> Int -> Int -> String
tipoTriangulo x y z | x == y && y == z = "Equilatero"
                    | x /= y && y /= z && x /= z = "Escaleno"
                    | otherwise = "Isosceles"
-- 16)
ehPar :: Int -> Bool
ehPar x = mod x 2 == 0