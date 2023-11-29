module Math
 (phi, mulInverse)
 where

-- bestimmt phi, welches die modulo-Umgebung beschreibt
phi :: Int -> Int -> Int
phi p q = (p-1) * (q-1)

-- berechnet das multiplikative Inverse der Zahl x in der modulo-Umgebung m
mulInverse :: Int -> Int -> Int
mulInverse x m = mod (fst(erwEuklAlgo x m)) m

-- Nutzung des erweiterten euklidischen Algorithmus zur bestimmung von (x,y) auf Basis der Gleichung:
-- ax + by = GGT(a,b)
erwEuklAlgo :: Int -> Int -> (Int, Int)
erwEuklAlgo a b
        | b == 0 = (1, 0)
        | otherwise = (aNew, bNew)
        where aNew = snd temp
              bNew = fst temp - div a b * snd temp
              temp = erwEuklAlgo b (mod a b)

-- verschlüsselt einen Char mithilfe des pubKeys
encrypt :: (Int, Int) -> String -> Int
encrypt (a, b) m = mod (m ** a)