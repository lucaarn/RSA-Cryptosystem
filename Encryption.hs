module Encryption where

import Numeric (showHex)
import Data.Char (ord)
import Control.Monad (replicateM)
import Math

-- wandelt eine Dezimalzahl in eine Hexadezimalzahl um (Rückgabe als String)
decToHex :: (Show a, Integral a) => a -> String
decToHex x = showHex x ""

-- konvertiert jeden Buchstaben eines gegebenen Strings in den zugehörigen hexadezimalen ASCII-Wert
-- map wendet auf jedes Zeichen des Strings die verkettete Funktion aus ord (Zeichen in dezimal ASCII) und decToHex an
-- Rückgabe als Array der ASCII-Werte
stringToHexValues :: String -> [String]
stringToHexValues = map (decToHex . ord)

encode :: String -> Int -> Int
encode s keyLength = keyLength

addPadding :: [String] -> Int -> IO [String]
addPadding s keyLength = do
  let paddingLength = div keyLength 16 - length s - 2
  randomIntegers <- replicateM paddingLength (randomInt(1, 254))
  return $ "02" : map decToHex randomIntegers ++ ["ff"] ++ s