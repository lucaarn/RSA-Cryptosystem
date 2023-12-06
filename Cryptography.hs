module Cryptography where
  
import Transcoding
import Key
import Math

-- wandelt ein Array aus 8-Bit Hex-Strings in einen Integer-Wert um
octetStreamToIntPrimitive :: [String] -> Integer
octetStreamToIntPrimitive octStr = hexToDec (concat octStr)

-- wandelt einen Integer-Wert in ein Array aus 8-Bit Hex-Strings um
intToOctetStreamPrimitive :: Integer -> [String]
intToOctetStreamPrimitive intMes = split 2 ("000" ++ decToHex intMes)

-- führt mathematische Operation zur Verschlüsselung durch
rsaEncryptionPrimitive :: Key -> Integer -> Integer
rsaEncryptionPrimitive (Public n e) m = modExp n e m 1

-- führt mathematische Operation zur Entschlüsselung durch
rsaDecryptionPrimitive :: Key -> Integer -> Integer
rsaDecryptionPrimitive (Private n d) c = modExp n d c 1

-- Verschlüselung eines Strings mithilfe des Public Keys:
-- Eingabestring durch encode zu 8-Bit Hex-String Array inkl. Padding umwandeln
-- Hex-String Array als Integer-Zahl darstellen
-- Verschlüsselungsoperation durchführen
-- Integer-Zahl als Hex-String Array darstellen und ausgeben
encrypt :: String -> Key -> IO [String]
encrypt str (Public n e) = do
  let keyLength = calcKeyLength (Public n e)
  em <- encode str keyLength
  let m = octetStreamToIntPrimitive em
  let c = rsaEncryptionPrimitive (Public n e) m
  return $ intToOctetStreamPrimitive c

-- Entschlüsselung eines Hex-String Arrays mithilfe des Private Keys:
-- Hex-String Array als Integer-Zahl darstellen
-- Entschlüsselungsoperation durchführen
-- Entschlüsselten Integer-Wert als Hex-String Array darstellen
-- Padding entfernen und in ASCII-Text wandeln durch decode
decrypt :: Key -> [String] -> IO String
decrypt (Private n d) octetStr = do
  let c = octetStreamToIntPrimitive octetStr
  let m = rsaDecryptionPrimitive (Private n d) c
  let em = intToOctetStreamPrimitive m
  decode em