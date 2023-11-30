module Cryptography where
  
import Transcoding

octetStreamToIntPrimitive :: [String] -> Int
octetStreamToIntPrimitive em = hexToDec (concat em)