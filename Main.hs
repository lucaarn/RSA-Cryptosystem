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

  encryptionOutput <- encrypt "Hallo, ich bin Luca" pub1
  
  decryptionOutput <- decrypt priv1 encryptionOutput
  print decryptionOutput