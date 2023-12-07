module Transcoding where

import Control.Monad (replicateM)
import Math
import Numeric (showHex, readHex)
import Data.Char (ord, chr)

-- wandelt eine Dezimalzahl in eine Hexadezimalzahl um (Rückgabe als String)
decToHex :: (Show a, Integral a) => a -> String
decToHex dec = do
  let hex = showHex dec ""
  if length hex == 1 then "0" ++ hex else hex

hexToDec :: (Integral a) => String -> a
hexToDec hex = case readHex hex of
               (dec, _):_ -> dec
               _          -> error "Invalid hex"

-- konvertiert jeden Buchstaben eines gegebenen Strings in den zugehörigen hexadezimalen ASCII-Wert
-- map wendet auf jedes Zeichen des Strings die verkettete Funktion aus ord (Zeichen in dezimal ASCII) und decToHex an
-- Rückgabe als Array der ASCII-Werte
stringToOctetStream :: String -> [String]
stringToOctetStream = map (decToHex . ord)

-- Ein String wird in ein Array aus n-langen Strings gesplittet
split :: Int -> String -> [String]
split n str = case splitAt n str of
              (a, b) | null a    -> []
                     | otherwise -> a : split n b

-- Generiert einen PaddingString, der dafür sorgt, dass nicht erkennbar ist, wie lang das eigentliche Wort ist
generatePaddingString :: Int -> Int -> Int -> IO [String]
generatePaddingString k mLen mode = do
  let paddingLength = div k 8 - mLen - 3
  if mode == 0
    then do
      randomIntegers <- replicateM paddingLength (randomInt(1, 254))
      return $ map decToHex randomIntegers
    else
      return $ replicate paddingLength "FF"

-- Wandelt ein Wort in einen verschlüsseltes HexArray um
-- In das verschlüsselte HexArray wird bereits ein PaddingString eingebunden
-- Decryption encoding, wenn mode 0, sonst Signature encoding
encode :: String -> Int -> Int -> IO [String]
encode m keyLength mode = do
  paddingString <- generatePaddingString keyLength (length m) mode
  let message = stringToOctetStream m
  if mode == 0
    then return $ ["00", "02"] ++ paddingString ++ ["00"] ++ message
    else return $ ["00", "01"] ++ paddingString ++ ["00"] ++ message

-- Wandelt ein veschlüsseltes HexArray in einen entschlüsselten StringText um
-- zuerst wird dabei der Paddingteil entfernt
-- Anschließend wird der HexString zu einem Integer
-- der Integer wird mittels ASCII-Werten in einen String umgewandelt
decode :: [String] -> IO String
decode em = do
  if head em /= "00" || em!!1 /= "02"
    then error "decryption error"
    else do
      let mesTemp = drop 2 em
      if length (takeWhile (/= "00") mesTemp) < 8
        then error "decryption error"
        else do
          let mes = (drop 1 . dropWhile (/= "00")) mesTemp
          let decMes = map hexToDec mes
          let ascii = map (chr . fromIntegral) decMes
          return ascii