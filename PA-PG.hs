-- n_esimo termo de uma progressao aritmetica (PA)
nT_PA :: Double -> Double -> Int -> Double
nT_PA pT r n = pT + (fromIntegral (n-1))*r
-- soma dos n_esimo termos de uma (PA)
sT_PA :: Double -> Double -> Int -> Double
sT_PA pT r n = ((fromIntegral n)*(pT + nT_PA pT r n))/2
-- n_esimo termo de uma progressao geometrica (PG)
nT_PG :: Double -> Double -> Int -> Double
nT_PG pT q n = pT * q^(n-1)
-- soma dos n_esimo termos de uma (PG)
sT_PG :: Double -> Double -> Int -> Double
sT_PG pT q n = (pT * (q^n - 1))/(q-1)
