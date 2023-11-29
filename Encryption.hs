module Encryption where

import Numeric (showHex)
import Data.Char (ord)

-- wandelt eine Dezimalzahl in eine Hexadezimalzahl um (Rückgabe als String)
decToHex :: (Show a, Integral a) => a -> String
decToHex x = showHex x ""

-- konvertiert jeden Buchstaben eines gegebenen Strings in den zugehörigen hexadezimalen ASCII-Wert
-- map wendet auf jedes Zeichen des Strings die verkettete Funktion aus ord (Zeichen in dezimal ASCII) und decToHex an
-- Rückgabe als Array der ASCII-Werte
stringToHex :: String -> [String]
stringToHex = map (decToHex . ord)