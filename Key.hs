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
  let p = 11259212850837339544665486601151895674218087499936576568948099017161149758785249146521965787431768878642480241801108694817806793678118718163058968771837853
  let q = 10158157686711727104043556458760215488498836845270304181991884234457355206194067694826191348302641023903724387445565873484037690377184734712923316357641803
  let (n, e, d) = getKeyComponents p q
  return (Public {n = n, e = e}, Private {n = n , d = d})

--bestimmt die Bitlänge eines Schlüssels
calcKeyLength :: Key -> Int
calcKeyLength (Public n _) = floor $ logBase 2 (fromIntegral n :: Double) + 1