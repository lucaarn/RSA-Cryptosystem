module Math
 (phi, inverseE, calculateInverse)
 where

phi :: Int -> Int -> Int
phi p q = (p-1) * (q-1)

inverseE :: Int -> Int -> Int
inverseE e phiN = mod (fst(calculateInverse e phiN)) phiN

calculateInverse :: Int -> Int -> (Int, Int)
calculateInverse a b
        | b == 0 = (1, 0)
        | otherwise = (aNew, bNew)
        where aNew = snd temp
              bNew = fst temp - div a b * snd temp
              temp = calculateInverse b (mod a b)