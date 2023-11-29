module Main where

import Math
import Numeric (showHex)

main :: IO()
main = do
  let p = 11
  let q = 13
  let n = p * q
  let phiN = phi p q
  let e = 65537
  let invE = mulInverse e phiN
  let pubKey = (e, n)
  let privKey = (invE, n)

  print pubKey
  print privKey