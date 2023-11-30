module Cryptography where
  
import Transcoding

octetStreamToIntPrimitive :: [String] -> Int
octetStreamToIntPrimitive em = hexToDec (concat em)

rsaEncryptionPrimitive :: (Int, Int) -> Int -> Int
rsaEncryptionPrimitive (n, e) m = m