module Main where

import Math
import Transcoding
import Cryptography

main :: IO()
main = do
  let p = 999999999959
  let q = 999999999989
  let n = p * q
  let phiN = phi p q
  let e = 65537
  let invE = mulInverse e phiN
  let pubKey = (e, n)
  let privKey = (invE, n)

  print pubKey
  print privKey

  let keyLength = calcKeyLength pubKey
  print keyLength
  let hexString = stringToHexValues "f"
  print hexString

  output <- encode "f" keyLength
  print output
  
  let intPrim = octetStreamToIntPrimitive output
  print intPrim