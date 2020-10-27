relatorio :: Int -> IO()
relatorio x | x < 1 || x > 12 = error "Mês invalido"
            | otherwise = putStr (ppHeader ++ ppTable x 1 ++ ppFooter x)

ppHeader :: String
ppHeader = unlines [ "\n           Tabela de vendas",
                       "Mês            Quantidade            R$" ]

ppTable :: Int -> Int -> String
ppTable x y | y < x = pRow y ++ "\n" ++ ppTable x (y+1)
            | otherwise = pRow y ++ "\n\n"

pRow :: Int -> String
pRow x | x == 1  = ppWord "Janeiro" 19   ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 2  = ppWord "Fevereiro" 19 ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 3  = ppWord "Março" 19     ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 4  = ppWord "Abril" 19     ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 5  = ppWord "Maio" 19      ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 6  = ppWord "Junho" 19     ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 7  = ppWord "Julho" 19     ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 8  = ppWord "Agosto" 19    ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 9  = ppWord "Setembro" 19  ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 10 = ppWord "Outubro" 19   ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 11 = ppWord "Novembro" 19  ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)
       | x == 12 = ppWord "Dezembro" 19  ++ ppWord (show (vendas x)) 14 ++ "R$ " ++ show (fromIntegral(vendas x) * preco)

ppWord :: String -> Int -> String
ppWord xs x = addSpace xs (x - length xs)

addSpace :: String -> Int -> String
addSpace xs x | x <= 0 = xs
              | otherwise = addSpace xs (x-1) ++ " " 

ppFooter :: Int -> String
ppFooter x = unlines [ ppWord "Total" 19 ++ ppWord (show (totalVendas x)) 14 ++ "R$ " ++ show (totalReceita x),
                       ppWord "Maior" 19 ++ ppWord (show (maiorVendas x)) 14,
                       ppWord "Menor" 19 ++ ppWord (show (menorVendas x)) 14,
                       ppWord "Venda Zerada" 19 ++ ppWord (show 0) 14,
                       "",
                       ppWord "Preço do Produto" 19 ++ ppWord ("R$ " ++ show preco) 14,
                       "\n" ]

totalVendas :: Int -> Int
totalVendas x = somadorVenda x 1

somadorVenda :: Int -> Int -> Int
somadorVenda x y | y <= x = vendas y + somadorVenda x (y+1)
                 | otherwise = 0

totalReceita :: Int -> Float
totalReceita x = fromIntegral (totalVendas x) * preco

maiorVendas :: Int -> Int
maiorVendas x = maior x 0

maior :: Int -> Int -> Int
maior x y | x == 0 = y
          | vendas x > y = maior (x-1) (vendas x)
          | otherwise = maior (x-1) y

menorVendas :: Int -> Int
menorVendas x = menor x (maxBound :: Int)

menor :: Int -> Int -> Int
menor x y | x == 0 = y
          | vendas x < y = menor (x-1) (vendas x)
          | otherwise = menor (x-1) y

-- A seguinte função recebe um número inteiro n (n>=1 e n<=12) que
-- representa um determinado mês e devolve a quantidade de vendas
-- do produto neste mês.
vendas :: Int -> Int
vendas 1 = 20
vendas 2 = 32
vendas 3 = 21
vendas 4 = 60
vendas 5 = 25
vendas 6 = 12
vendas 7 = 52
vendas 8 = 28
vendas 9 = 29
vendas 10 = 40
vendas 11 = 50
vendas 12 = 33

-- Variável que representa o preço do produto (R$ 12,00)
preco :: Float
preco = 12.0
