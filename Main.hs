module Main where

import Math
import Transcoding
import Cryptography
import Key
import Signature

alice :: IO (Key, Key)
alice = getKeyPair 1024

bob :: IO (Key, Key)
bob = getKeyPair 1024

main :: IO()
main = do
  aliceKeyPair <- alice
  bobKeyPair <- bob

  let (aPub, aPriv) = aliceKeyPair
  let (bPub, bPriv) = bobKeyPair

  putStrLn "Zu verschlüsselndes Wort: (ASCII-Zeichen only)"
  word <- getLine

  encryptionOutput <- encrypt word aPub
  
  decryptionOutput <- decrypt aPriv encryptionOutput
  print decryptionOutput

  signed <- sign word bPriv
  
  output <- verify bPub word signed
  print output