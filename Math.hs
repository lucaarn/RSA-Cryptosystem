module Math where

import System.Random (randomRIO)
import MillerRabin

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

-- gibt einen zufälligen Integer-Wert im Intervall [min, max] zurück
randomInt :: (Int, Int) -> IO Int
randomInt (minVal, maxVal) = randomRIO(minVal, maxVal)

-- berechnet x = m^e mod m
modExp :: Integral a => a -> a -> a -> a -> a
modExp n e m x | e <= 0    = x
               | otherwise = modExp n eNew mNew xNew
               where eNew = div e 2
                     mNew = mod (m * m) n
                     xNew | mod e 2 == 1 = mod (m * x) n
                          | otherwise    = x

-- testet, ob es sich bei bei x um eine Primzahl handelt
-- Nutzung des Miller-Rabin-Algorithmus (Quelle in MillerRabin.hs)
isPrime :: Integer -> IO Bool
isPrime x = do
  let randomInts = replicate 5 (randomRIO (2, x - 1))
  witnesses <- sequence randomInts
  let result = map (millerRabinPrimality x) witnesses
  return $ and result

-- gibt eine zufällige Primzahl zurück, welche genau n Bits zur Darstellung benötigt
-- n ist dabei die Länge des Schlüssels in Bits (standardmäßig 1024)
randomPrime :: Int -> IO Integer
randomPrime keyLengthBits = do
  let minVal = ceiling $ sqrt 2 * 2^(keyLengthBits - 1)
  let maxVal = 2^keyLengthBits - 1
  num <- randomRIO(minVal, maxVal)
  isNumPrime <- isPrime num
  if isNumPrime then return num else randomPrime keyLengthBits