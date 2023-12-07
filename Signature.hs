module Signature where

import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import Cryptography
import Transcoding
import Key
import Math
import Control.Exception (SomeException)

-- Wendet SHA-1 hash-Funktion auf eingebenes Wort an
sha1Hash :: String -> Digest SHA1
sha1Hash input = hash (B.pack input)

-- Prüft, ob Hash-Wert des erhaltenen Wortes mit dem Hash-Wert aus der Signatur übereinstimmt
verifyHash :: Digest SHA1 -> String -> Bool
verifyHash correct sent = show correct == sent

-- führt mathematische Operation durch, um gehashte Nachricht zu signieren
rsaSignaturePrimitive :: Key -> Integer -> Integer
rsaSignaturePrimitive (Private n d) m | m < 0 || m > n - 1 = error "message representative out of range"
                                      | otherwise          = modExp n d m 1
rsaSignaturePrimitive (Public _ _) _ = error "use private key"

-- führt mathematische Operation durch, um signierte Nachricht zu verifizieren
rsaVerificationPrimitive :: Key -> Integer -> Either SomeException Integer
rsaVerificationPrimitive (Public n e) s | s < 0 || s > n - 1 = Left $ error "signature representative out of range"
                                        | otherwise          = Right $ modExp n e s 1
rsaVerificationPrimitive (Private _ _) _ = error "use public key"

-- Signatur des eingebenen Wortes mit eigenem Private Key:
-- Hash-Funktion wird auf Wort angewandt
-- Wort durch encode zu 8-Bit Hex-String Array inkl. Padding umwandeln
-- Hex-String Array als Integer-Zahl darstellen
-- Verschlüsselungsoperation durchführen
-- Integer-Zahl als Hex-String Array darstellen und ausgeben
sign :: String -> Key -> IO [String]
sign str (Private n d) = do
  let keyLength = calcKeyLength (Public n d)
  let hashStr = show (sha1Hash str)
  if length hashStr > div keyLength 8 - 11
    then error "message too long"
    else do
      em <- encode (show (sha1Hash str)) keyLength 1
      let m = octetStreamToIntPrimitive em
      let s = rsaSignaturePrimitive (Private n d) m
      let output = intToOctetStreamPrimitive s (div keyLength 8) 0
      return output
sign _ (Public _ _) = error "use private key"

-- Verifizieren der Signatur mit fremden Public Key:
-- Hex-String Array als Integer-Zahl darstellen
-- Entschlüsselungsoperation durchführen
-- Entschlüsselten Integer-Wert als Hex-String Array darstellen
-- Padding entfernen und in ASCII-Text wandeln durch decode
-- Überprüfen, ob Hash-Wert des entschlüsselten Wortes mit erhaltenem Hash-Wert übereinstimmt
verify :: Key -> String -> [String] -> IO String
verify (Public n e) mes octetStr = do
  let keyLength = calcKeyLength (Public n e)
  if length octetStr /= div keyLength 8
    then error "invalid signature"
    else do
      let s = octetStreamToIntPrimitive octetStr
      let m = rsaVerificationPrimitive (Public n e) s
      case m of
        Left ex -> error "invalid signature"
        Right val -> do 
          let em = intToOctetStreamPrimitive val (div keyLength 8) 1
          let dec = decode em
          case dec of
            Left ex -> error "invalid signature"
            Right val -> do
              if verifyHash (sha1Hash mes) val
                then return "valid signature"
                else return "invalid signature"