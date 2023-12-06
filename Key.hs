module Key where

import Math

data Key = Public {n :: Integer, e :: Integer}
         | Private {n :: Integer, d :: Integer}
     deriving Show

--liefert den allgemeinen öffetnlichen Exponenten e, der immer 65537 ist
getPublicExponent :: Integer
getPublicExponent = 65537

--liefert alle einzelnen Komponenten eines Schlüssels
getKeyComponents :: Integer -> Integer -> (Integer, Integer, Integer)
getKeyComponents p q = (n, e, d)
  where n = p * q
        m = phi p q
        e = getPublicExponent
        d = mulInverse e m

--liefert ein bestimmtes Schlüsselpaar, bestehend aus einem privaten und einem öffentlichem Teil
getKeyPair :: Int -> IO (Key, Key)
getKeyPair keyLength = do
  let keyLengthBits = div keyLength 2
  p <- randomPrime keyLengthBits
  q <- randomPrime keyLengthBits
  let (n, e, d) = getKeyComponents p q
  return (Public {n = n, e = e}, Private {n = n , d = d})

--bestimmt die Bitlänge eines Schlüssels
calcKeyLength :: Key -> Int
calcKeyLength (Public n _) = floor $ logBase 2 (fromIntegral n :: Double) + 1