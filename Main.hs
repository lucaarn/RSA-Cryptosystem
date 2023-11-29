module Main where

import Math

main :: IO()
main = do
  let p = 11
  let q = 13
  let phiN = phi p q
  let e = 65537
  let invE = inverseE e phiN

  print invE

