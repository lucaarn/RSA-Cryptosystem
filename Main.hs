module Main where

import Math
import Transcoding
import Cryptography
import Key

alice :: IO (Key, Key)
alice = getKeyPair 1024

main :: IO()
main = do
  keyPair1 <- alice

  let (pub1, priv1) = keyPair1

  let keyLength = calcKeyLength pub1
  print keyLength
  let hexString = stringToHexValues "f"
  print hexString

  output <- encode "f" keyLength
  print output
  
  let intPrim = octetStreamToIntPrimitive output
  print intPrim
  
  let rsaPrim = rsaEncryptionPrimitive pub1 intPrim
  print rsaPrim