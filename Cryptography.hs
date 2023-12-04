module Cryptography where
  
import Transcoding
import Key
import Math

octetStreamToIntPrimitive :: [String] -> Integer
octetStreamToIntPrimitive em = hexToDec (concat em)

intToOctetStreamPrimitive :: Integer -> [String]
intToOctetStreamPrimitive c = split 2 (decToHex c)

rsaEncryptionPrimitive :: Key -> Integer -> Integer
rsaEncryptionPrimitive (Public n e) m = modExp n e m 1

encrypt :: String -> Key -> IO [String]
encrypt str (Public n e) = do
  let keyLength = calcKeyLength (Public n e)
  em <- encode str keyLength
  let m = octetStreamToIntPrimitive em
  let c = rsaEncryptionPrimitive (Public n e) m
  return $ intToOctetStreamPrimitive c