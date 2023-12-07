module Cryptography where
  
import Transcoding
import Key
import Math

-- wandelt ein Array aus 8-Bit Hex-Strings in einen Integer-Wert um
octetStreamToIntPrimitive :: [String] -> Integer
octetStreamToIntPrimitive octStr = hexToDec (concat octStr)

-- wandelt einen Integer-Wert in ein Array aus 8-Bit Hex-Strings um
intToOctetStreamPrimitive :: Integer -> Int -> Int -> [String]
intToOctetStreamPrimitive intMes len mode | intMes >= 256^len = error "integer too large"
                                          | mode == 0         = split 2 (decToHex intMes)
                                          | otherwise         = split 2 ("000" ++ decToHex intMes)

-- führt mathematische Operation zur Verschlüsselung durch
rsaEncryptionPrimitive :: Key -> Integer -> Integer
rsaEncryptionPrimitive (Public n e) m | m < 0 || m > n - 1 = error "message representative out of range"
                                      | otherwise          = modExp n e m 1
rsaEncryptionPrimitive (Private _ _) _ = error "use public key"

-- führt mathematische Operation zur Entschlüsselung durch
rsaDecryptionPrimitive :: Key -> Integer -> Integer
rsaDecryptionPrimitive (Private n d) c | c < 0 || c > n - 1 = error "message representative out of range"
                                       | otherwise          = modExp n d c 1
rsaDecryptionPrimitive (Public _ _) _ = error "use private key"

-- Verschlüselung eines Strings mithilfe des Public Keys:
-- Eingabestring durch encode zu 8-Bit Hex-String Array inkl. Padding umwandeln
-- Hex-String Array als Integer-Zahl darstellen
-- Verschlüsselungsoperation durchführen
-- Integer-Zahl als Hex-String Array darstellen und ausgeben
encrypt :: String -> Key -> IO [String]
encrypt str (Public n e) = do 
  let keyLength = calcKeyLength (Public n e)
  if length str > div keyLength 8 - 11
    then error "message too long"
    else do
      putStrLn "\n\n Beginn Verschlüsselung"

      em <- encode str keyLength 0
      putStrLn "Wort als Hex-String mit Padding"
      print em

      let m = octetStreamToIntPrimitive em
      putStrLn "\nHex-String als Dezimalzahl"
      print m

      let c = rsaEncryptionPrimitive (Public n e) m
      putStrLn "\nVerschlüsseltes Wort als Dezimalzahl"
      print c

      let output = intToOctetStreamPrimitive c (div keyLength 8) 0
      putStrLn "\nVerschlüsseltes Wort als Hex-String"
      print output

      return output
encrypt _ (Private _ _) = error "use public key"

-- Entschlüsselung eines Hex-String Arrays mithilfe des Private Keys:
-- Hex-String Array als Integer-Zahl darstellen
-- Entschlüsselungsoperation durchführen
-- Entschlüsselten Integer-Wert als Hex-String Array darstellen
-- Padding entfernen und in ASCII-Text wandeln durch decode
decrypt :: Key -> [String] -> IO String
decrypt (Private n d) octetStr = do
  let keyLength = calcKeyLength (Private n d)
  if length octetStr /= div keyLength 8
    then error "decryption error"
    else do
      putStrLn "\n\n Beginn Entschlüsselung"

      let c = octetStreamToIntPrimitive octetStr
      putStrLn "Verschlüsseltes Wort als Dezimalzahl"
      print c

      let m = rsaDecryptionPrimitive (Private n d) c
      putStrLn "\nEntschlüsseltes Wort als Dezimalzahl"
      print m

      let em = intToOctetStreamPrimitive m (div keyLength 8) 1
      putStrLn "\nEntschlüssteltes Wort als Hex-String mit Padding"
      print em

      putStrLn "\nEntschlüsseltes Wort"
      let output = decode em
      case output of
        Left ex -> error "encryption error"
        Right val -> return val
decrypt (Public _ _) _ = error "use private key"