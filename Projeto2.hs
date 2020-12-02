module Mercado where

type Nome = String
type Preco = Int
type CodigoBarra = Int
type BancoDeDados = [ (CodigoBarra, Nome, Preco) ]

-- Função Principal!
imprimirConta :: [CodigoBarra] -> IO( )
imprimirConta lista = putStr (formatarConta (fazerConta lista))


bd :: BancoDeDados
bd = [ (1001,"Refrigerante",450),
      (1002,"Leite",320),
      (1003,"Biscoito",200),
      (1004,"Suco",989),
      (1005,"Arroz",345),
      (1006,"Feijao",780)]

-- Questão 1
buscarBDaux :: CodigoBarra -> BancoDeDados -> (Nome, Preco)
buscarBDaux _ [] = ("",0)
buscarBDaux x ((cd,nm,pr):bd) | x == cd = (nm,pr)
                              | otherwise = buscarBDaux x bd

-- Questão 2
buscarBD :: CodigoBarra -> (Nome, Preco)
buscarBD x = buscarBDaux x bd

-- Questão 3
fazerConta :: [CodigoBarra] -> [(Nome, Preco)]
fazerConta [x] = [buscarBD x]
fazerConta (x:xs) = [buscarBD x] ++ fazerConta xs

-- Questão 4
dividir :: Int -> String
dividir x = (show (x `div` 100)) ++ "." ++(show (x `mod` 100))

-- Questão 5
repetir :: Int -> String -> String
repetir 1 r = r 
repetir x r = r ++ repetir (x-1) r

tamanhoLinha :: Int
tamanhoLinha = 30
-- Questão 6
formatarLinha :: (Nome, Preco) -> String
formatarLinha (nm, pr) = nm ++ repetir (tamanhoLinha - length nm - length (dividir pr)) "." ++ dividir pr ++ "\n"

-- Questão 7
formatarLinhas :: [(Nome, Preco)] -> String
formatarLinhas [x] = formatarLinha x
formatarLinhas (x:xs) = formatarLinha x ++ formatarLinhas xs

-- Questão 8
calcularTotal :: [(Nome, Preco)] -> Int
calcularTotal [(_,x)] = x
calcularTotal ((_,x):xs) = x + calcularTotal xs

-- Questão 9
formatarTotal :: Int -> String
formatarTotal x = "Total:" ++ repetir (tamanhoLinha - 6 - length (dividir x)) "." ++ dividir x ++ "\n"

-- Questão 10
formatarConta :: [(Nome, Preco)] -> String
formatarConta xs = formatarLinhas xs ++ formatarTotal (calcularTotal xs)
