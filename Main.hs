module Main where

import Math
import Encryption

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
  
  output <- addPadding hexString keyLength
  print output