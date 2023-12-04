module Math where

import System.Random (randomRIO)

-- berechnet Φ(N) auf der Grundlage, dass p und q Primzahlen sind
phi :: Integer -> Integer -> Integer
phi p q = (p-1) * (q-1)

-- berechnet das multiplikative Inverse der Zahl x in der modulo-Umgebung m
mulInverse :: Integer -> Integer -> Integer
mulInverse x m = mod (fst(extEucAlgo x m)) m

-- Nutzung des erweiterten euklidischen Algorithmus zur bestimmung von (x,y) auf Basis der Gleichung:
-- ax + by = GGT(a,b)
extEucAlgo :: Integer -> Integer -> (Integer, Integer)
extEucAlgo a b
        | b == 0 = (1, 0)
        | otherwise = (aNew, bNew)
        where aNew = snd temp
              bNew = fst temp - div a b * snd temp
              temp = extEucAlgo b (mod a b)

randomInt :: (Int, Int) -> IO Int
randomInt (min, max) = randomRIO(min, max)

modExp :: Integral a => a -> a -> a -> a -> a
modExp n e m x | e <= 0    = x
               | otherwise = modExp n eNew mNew xNew
                 where eNew = div e 2
                       mNew = mod (m * m) n
                       xNew | mod e 2 == 1 = mod (m * x) n
                            | otherwise    = x