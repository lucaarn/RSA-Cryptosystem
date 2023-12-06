module Main where

import Math
import Transcoding
import Cryptography
import Key
import Signature

alice :: IO (Key, Key)
alice = getKeyPair 1024

main :: IO()
main = do
  keyPair1 <- alice
  let (pub, priv) = keyPair1

  putStrLn "Zu verschlüsselndes Wort: (ASCII-Zeichen only)"
  word <- getLine

  encryptionOutput <- encrypt word pub
  
  decryptionOutput <- decrypt priv encryptionOutput
  print decryptionOutput

  signed <- sign "hello" priv
  
  output <- verify pub "hello" signed
  print output