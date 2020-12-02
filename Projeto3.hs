-- Função Principal!
showLCD :: Int -> IO()
showLCD x = putStr (tolcd x)

nove,cinco,um,dois,tres,quatro,seis,sete,oito,zer :: [Int]
nove = [4,1,4,2,1,2,1]
cinco = [4,2,3,2,4]
um = [0,2,1,2,1,2,1,2,1,2,1]
dois = [3,2,5,2,3]
tres = [3,2,4,2,4]
quatro = [1,1,2,1,4,2,1,2,1]
seis = [4,2,4,1,4]
sete = [3,2,1,2,1,2,1,2,1]
oito = [4,1,5,1,4]
zer = [4,1,2,1,2,1,4]

-- Questão 1
toString :: [Int] -> String
toString [x] = repetir x "*"
toString (x:y:xs) = repetir x "*" ++ repetir y " " ++ toString xs

repetir :: Int -> String -> String
repetir 0 s = ""
repetir x s = s ++ repetir (x-1) s

-- Questão 2
type Linha = String
toLinhas :: String -> [Linha]
toLinhas [] = []
toLinhas (x:y:z:xs) = [([x] ++ [y] ++ [z])] ++ toLinhas xs

-- Questão 3
showLinhas :: [Linha] -> String
showLinhas xs = unlines xs

-- Questão 4
juntaLinhas :: [Linha] -> [Linha] -> [Linha]
juntaLinhas [x] [y] = [x ++ " " ++ y]
juntaLinhas (x:xs) (y:ys) = [x ++ " " ++ y] ++ juntaLinhas xs ys

-- Questão 5
tolcd :: Int -> String
tolcd x | (abs x) <= 9   = showLinhas (toLCDNumber x)
        | (abs x) <= 99  = showLinhas (juntaLinhas (toLCDNumber (x `div` 10)) (toLCDNumber (x `mod` 10)))
        | (abs x) <= 999 = showLinhas (juntaLinhas (juntaLinhas (toLCDNumber (x `div` 100)) (toLCDNumber ((x `mod` 100) `div` 10))) (toLCDNumber (x `mod` 10)))
        | otherwise = "Somente inteiros com 3 ou menos digitos\n"

numeros :: [[Int]]
numeros = [zer,um,dois,tres,quatro,cinco,seis,sete,oito,nove]

toLCDNumber :: Int -> [Linha]
toLCDNumber x = toLinhas (toString (numeros!!x))

-- Questão 6
toCompact :: String -> [Int]
toCompact xs = toCompactAux xs '*' 0 []

toCompactAux :: String -> Char -> Int -> [Int] -> [Int]
toCompactAux [] _ x xs = xs ++ [x]
toCompactAux (s:ss) c x xs | s == c = toCompactAux ss c (x+1) xs
                           | otherwise = toCompactAux ([s]++ss) s 0 (xs++[x])
