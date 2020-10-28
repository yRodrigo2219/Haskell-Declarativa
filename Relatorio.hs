-- recebe um inteiro que representa ate que mês deve se imprimir o relatorio
-- e retorna a sua impressão
relatorio :: Int -> IO()
relatorio x | x < 1 || x > 12 = error "Mês invalido"
            | otherwise = putStr (ppHeader ++ ppTable x 1 ++ ppFooter x)

-- retorna o cabeçalho da tabela de relatorio
ppHeader :: String
ppHeader = unlines [ "",
                     ppWordRight "Tabela de vendas" 27,
                     ppWordLeft "Mês" 15 ++ ppWordLeft "Quantidade" 22 ++ "R$" ]

-- recebe um inteiro que representa ate que mês deve se olhar no relatorio,
-- recursivamente, e recebe qual o mes inicial para retornar as linhas da tabela
ppTable :: Int -> Int -> String
ppTable x y | y < x = pRow y ++ "\n" ++ ppTable x (y+1)
            | otherwise = pRow y ++ "\n\n"

-- recebe um inteiro que representa o mês da tabela, retorna a linha da tabela correspondente
pRow :: Int -> String
pRow x = ppWordLeft (mes x) 19 ++ ppWordLeft (show (vendas x)) 14 ++ "R$ " ++ show (receita x)

-- recebe um inteiro que representa o mês da tabela, retorna o nome do mês
mes :: Int -> String
mes x | x == 1  = "Janeiro"
      | x == 3  = "Fevereiro"
      | x == 2  = "Março"
      | x == 4  = "Abril"
      | x == 5  = "Maio"
      | x == 6  = "Junho"
      | x == 7  = "Julho"
      | x == 8  = "Agosto"
      | x == 9  = "Setembro"
      | x == 10 = "Outubro"
      | x == 11 = "Novembro"
      | x == 12 = "Dezembro"

-- pretty print na palavra, colocando ela à esquerda
-- recebe a palavra a ser modificada e um inteiro que representa a
-- quantidade de caracteres que ela deve ter, adiciona espaços a direita
ppWordLeft :: String -> Int -> String
ppWordLeft xs x = addSpaceRight xs (x - length xs)

-- pretty print na palavra, colocando ela à direita
-- recebe a palavra a ser modificada e um inteiro que representa a
-- quantidade de caracteres que ela deve ter, adiciona espaços a esquerda
ppWordRight :: String -> Int -> String
ppWordRight xs x = addSpaceLeft xs (x - length xs)

-- função auxiliar que adiciona espaços em branco ao lado direito da palavra
addSpaceRight :: String -> Int -> String
addSpaceRight xs x | x <= 0 = xs
                   | otherwise = addSpaceRight xs (x-1) ++ " "

-- função auxiliar que adiciona espaços em branco ao lado esquerdo da palavra
addSpaceLeft :: String -> Int -> String
addSpaceLeft xs x | x <= 0 = xs
                  | otherwise = " " ++ addSpaceLeft xs (x-1)

-- recebe um inteiro que representa ate que mês deve se olhar no relatorio
-- para retornar informações extra sobre o relatorio
ppFooter :: Int -> String
ppFooter x = unlines [ ppWordLeft "Total" 19 ++ ppWordLeft (show (totalVendas x)) 14 ++ "R$ " ++ show (totalReceita x),
                       ppWordLeft "Maior" 19 ++ ppWordLeft (show (maiorVendas x)) 14,
                       ppWordLeft "Menor" 19 ++ ppWordLeft (show (menorVendas x)) 14,
                       ppWordLeft "Venda Zerada" 19 ++ ppWordLeft (show (vendaZerada x)) 14,
                       "",
                       ppWordLeft "Preço do Produto" 19 ++ ppWordLeft ("R$ " ++ show preco) 14,
                       "" ]

-- recebe um inteiro que representa ate que mês deve se olhar o relatorio
-- para retornar o total de vendas ( dentre os meses analizados )
totalVendas :: Int -> Int
totalVendas x = somadorVenda x 1

-- função auxiliar de totalVendas, olha recursivamente o relatorio, retornando
-- a soma da quantidade de vendas ( dentre os meses analizados )
somadorVenda :: Int -> Int -> Int
somadorVenda x y | y <= x = vendas y + somadorVenda x (y+1)
                 | otherwise = 0

-- recebe um inteiro que representa ate que mês deve se olhar o relatorio
-- para retornar o total gerado em receita ( dentre os meses analizados )
totalReceita :: Int -> Float
totalReceita x = fromIntegral (totalVendas x) * preco

-- recebe um inteiro que representa ate que mês deve se olhar o relatorio
-- para retornar qual a maior quantidade de vendas nos meses analizados
maiorVendas :: Int -> Int
maiorVendas x = maior x 0

-- função auxiliar de maiorVendas, olha recursivamente o relatorio, retornando
-- qual a maior quantidade de vendas ( dentre os messes analizados )
maior :: Int -> Int -> Int
maior x y | x == 0 = y
          | vendas x > y = maior (x-1) (vendas x)
          | otherwise = maior (x-1) y

-- recebe um inteiro que representa ate que mês deve se olhar o relatorio
-- para retornar qual a menor quantidade de vendas nos meses analizados
menorVendas :: Int -> Int
menorVendas x = menor x (maxBound :: Int)

-- função auxiliar de menorVendas, olha recursivamente o relatorio, retornando
-- qual a menor quantidade de vendas ( dentre os messes analizados )
menor :: Int -> Int -> Int
menor x y | x == 0 = y
          | vendas x < y = menor (x-1) (vendas x)
          | otherwise = menor (x-1) y

-- recebe um inteiro que representa ate que mês deve se olhar o relatorio
-- para retornar quantos meses houveram 0 vendas
vendaZerada :: Int -> Int
vendaZerada x = zerada x 0

-- função auxiliar de vendaZerada, olha recursivamente o relatorio, contando
-- quantas vendas zeradas se encontram
zerada :: Int -> Int -> Int
zerada x y | x == 0 = y
           | vendas x == 0 = zerada (x-1) (y+1)
           | otherwise = zerada (x-1) y


-- recebe um inteiro que representa o mês e devolve a receita do mês
receita :: Int -> Float
receita x = fromIntegral(vendas x) * preco

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
