module Signature where

import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import Cryptography
import Transcoding
import Key
import Math

sha1Hash :: String -> Digest SHA1
sha1Hash input = hash (B.pack input)

verifyHash :: Digest SHA1 -> String -> Bool
verifyHash correct sent = show correct == sent

rsaSignaturePrimitive :: Key -> Integer -> Integer
rsaSignaturePrimitive (Private n d) m | m < 0 || m > n - 1 = error "message representative out of range"
                                      | otherwise          = modExp n d m 1
rsaSignaturePrimitive (Public _ _) _ = error "use private key"

rsaVerificationPrimitive :: Key -> Integer -> Integer
rsaVerificationPrimitive (Public n e) s | s < 0 || s > n - 1 = error "signature representative out of range"
                                        | otherwise          = modExp n e s 1
rsaVerificationPrimitive (Private _ _) _ = error "use public key"

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

verify :: Key -> String -> [String] -> IO String
verify (Public n e) mes octetStr = do
  let keyLength = calcKeyLength (Public n e)
  if length octetStr /= div keyLength 8
    then error "invalid signature"
    else do
      let s = octetStreamToIntPrimitive octetStr
      let m = rsaVerificationPrimitive (Public n e) s
      let em = intToOctetStreamPrimitive m (div keyLength 8) 1
      dec <- decode em
      if verifyHash (sha1Hash mes) dec
        then return "valid signature"
        else return "invalid signature"