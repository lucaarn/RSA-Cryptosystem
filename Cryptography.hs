module Cryptography where
  
import Transcoding
import Key
import Math

octetStreamToIntPrimitive :: [String] -> Integer
octetStreamToIntPrimitive octStr = hexToDec (concat octStr)

intToOctetStreamPrimitive :: Integer -> [String]
intToOctetStreamPrimitive intMes = split 2 ("000" ++ decToHex intMes)

rsaEncryptionPrimitive :: Key -> Integer -> Integer
rsaEncryptionPrimitive (Public n e) m = modExp n e m 1

rsaDecryptionPrimitive :: Key -> Integer -> Integer
rsaDecryptionPrimitive (Private n d) c = modExp n d c 1

encrypt :: String -> Key -> IO [String]
encrypt str (Public n e) = do
  let keyLength = calcKeyLength (Public n e)
  em <- encode str keyLength
  let m = octetStreamToIntPrimitive em
  let c = rsaEncryptionPrimitive (Public n e) m
  return $ intToOctetStreamPrimitive c
  
decrypt :: Key -> [String] -> IO String
decrypt (Private n d) octetStr = do
  let c = octetStreamToIntPrimitive octetStr
  let m = rsaDecryptionPrimitive (Private n d) c
  let em = intToOctetStreamPrimitive m
  decode em