-- Utility
fat :: Integer -> Integer
fat x | x == 0 = 1
      | x > 0 = fat (x-1) * x

-- 4)
pot :: Int -> Int -> Int
pot k n = k^n
-- 5)
somaPares :: Int -> Int
somaPares n | n == 0 = 0
            | n > 0 && mod n 2 == 1 = somaPares (n-1)
            | n > 0 = somaPares (n-2) + n
-- 6)
succ :: Int -> Int
succ x = soma x 1

soma :: Int -> Int -> Int
soma x y = x + y
-- 7)
sb :: Int -> Double
sb n | n == 1 = 1
     | n > 1 = sb (n-1) + (1/(fromIntegral n))
-- 8)
e :: Double -> Integer -> Double
e x n | n == 0 = 1
      | n > 0 = e x (n-1) + ((x^n)/(fromIntegral (fat n)))
-- 10)
mdc :: Int -> Int -> Int
mdc a b | b == 0 = a
        | otherwise = mdc b (a `mod` b)
-- 11)
-- a)
sumQuad :: Integer -> Integer
sumQuad n | n == 1 = 1
          | n > 1 = sumQuad (n-1) + n^2
-- b)
sumFat :: Integer -> Integer
sumFat n | n == 1 = 1
         | n > 1 = sumFat (n-1) + fat n
-- 12)
tabuada :: Int -> String
tabuada n = unlines [ "Tabuada do " ++ show n , 
                      show n ++ " X 1  = " ++ show (n*1) ,
                      show n ++ " X 2  = " ++ show (n*2) ,
                      show n ++ " X 3  = " ++ show (n*3) ,
                      show n ++ " X 4  = " ++ show (n*4) ,
                      show n ++ " X 5  = " ++ show (n*5) ,
                      show n ++ " X 6  = " ++ show (n*6) ,
                      show n ++ " X 7  = " ++ show (n*7) ,
                      show n ++ " X 8  = " ++ show (n*8) ,
                      show n ++ " X 9  = " ++ show (n*9) ,
                      show n ++ " X 10 = " ++ show (n*10) ]