module Cryptography where
  
import Transcoding
import Key
import Math

octetStreamToIntPrimitive :: [String] -> Integer
octetStreamToIntPrimitive em = hexToDec (concat em)

rsaEncryptionPrimitive :: Key -> Integer -> Integer
rsaEncryptionPrimitive (Public n e) m = modExp n e m 1