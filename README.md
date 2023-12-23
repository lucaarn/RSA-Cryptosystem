# RSA Cryptosystem

## Overview

This Haskell project implements the RSA cryptosystem, offering a practical demonstration of the algorithm's core functionalities. While the implementation is for educational purposes, it extends beyond the basic mathematical principles to include key components often omitted in simpler versions.

## Key Features

- **Secure Padding Scheme:** Utilizes a proper padding scheme to enhance plaintext security.
- **Key Generation:** Generates RSA keys with appropriate bit-lengths and conventional parameters.
- **Digital Signatures:** Supports the creation and verification of digital signatures.
- **Efficient Operations:** Ensures fast encryption, decryption, and key generation processes.

## Disclaimer

This implementation, like many RSA examples on the internet, is not intended for secure cryptographic use. It serves as an educational resource to explore the various elements of the RSA system. Note that this code may have vulnerabilities and is susceptible to side-channel attacks.

## Project Details

- **Cryptography:** Encryption and decryption operations.
- **Key:** RSA key generation and key length calculation.
- **Math:** Mathematical functions for RSA operations.
- **MillerRabin:** Implementation of the Miller-Rabin primality test.
- **Signature:** Digital signature creation and verification.
- **Transcoding:** Hexadecimal encoding/decoding and string conversion.

## Usage

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/YourGitHubUsername/RSA-cryptosystem.git
   ```

2. **Build the Project:**
   ```bash
   stack build
   ```

3. **Run the Executable:**
   ```bash
   stack run
   ```

## Notes

- This code is written in vanilla Haskell without external libraries.