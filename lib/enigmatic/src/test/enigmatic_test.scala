                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package enigmatic

import soundness.*

import strategies.throwUnsafely
import charDecoders.utf8, charEncoders.utf8, textSanitizers.skip
import errorDiagnostics.stackTracesDiagnostics
import providers.javaStdlibProvider
import crypto.permitDisallowedCrypto   // the suite deliberately exercises weak crypto

import alphabets.hex.upperCase

object Tests extends Suite(m"Gastronomy tests"):

  val request: Text = t"""
    |-----BEGIN CERTIFICATE REQUEST-----
    |MIIB9TCCAWACAQAwgbgxGTAXBgNVBAoMEFF1b1ZhZGlzIExpbWl0ZWQxHDAaBgNV
    |BAsME0RvY3VtZW50IERlcGFydG1lbnQxOTA3BgNVBAMMMFdoeSBhcmUgeW91IGRl
    |Y29kaW5nIG1lPyAgVGhpcyBpcyBvbmx5IGEgdGVzdCEhITERMA8GA1UEBwwISGFt
    |aWx0b24xETAPBgNVBAgMCFBlbWJyb2tlMQswCQYDVQQGEwJCTTEPMA0GCSqGSIb3
    |DQEJARYAMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCJ9WRanG/fUvcfKiGl
    |EL4aRLjGt537mZ28UU9/3eiJeJznNSOuNLnF+hmabAu7H0LT4K7EdqfF+XUZW/2j
    |RKRYcvOUDGF9A7OjW7UfKk1In3+6QDCi7X34RE161jqoaJjrm/T18TOKcgkkhRzE
    |apQnIDm0Ea/HVzX/PiSOGuertwIDAQABMAsGCSqGSIb3DQEBBQOBgQBzMJdAV4QP
    |Awel8LzGx5uMOshezF/KfP67wJ93UW+N7zXY6AwPgoLj4Kjw+WtU684JL8Dtr9FX
    |ozakE+8p06BpxegR4BR3FMHf6p+0jQxUEAkAyb/mVgm66TyghDGC6/YkiKoZptXQ
    |98TwDIK/39WEB/V607As+KoYazQG8drorw==
    |-----END CERTIFICATE REQUEST-----
    """.s.stripMargin.show

  val pangram: Text = t"The quick brown fox jumps over the lazy dog"

  def run(): Unit =
    test(m"Sha256, Hex"):
      t"Hello world".digest[Sha2[256]].serialize[Hex]
    . assert(_ == t"64EC88CA00B268E5BA1A35678A1B5316D212F4F366B2477232534A8AECA37F3C")

    test(m"Md5, Base64"):
      import alphabets.base64.standard
      t"Hello world".digest[Md5].serialize[Base64]
    . assert(_ == t"PiWWCnnbxptnTNTsZ6csYg==")

    test(m"Sha1, Base64Url"):
      import alphabets.base64.url
      t"Hello world".digest[Sha1].data.serialize[Base64]
    . assert(_ == t"e1AsOh9IyGCa4hLN-2Od7jlnP14")

    test(m"Sha384, Base64"):
      import alphabets.base64.standard
      t"Hello world".digest[Sha2[384]].serialize[Base64]
    . assert(_ == t"kgOwxEOf0eauWHiGYze3xTKs1tkmAVDIAxjoq4wnzjMBifjflPuJDfHSmP82Bifh")

    test(m"Sha512, Base64"):
      import alphabets.base64.standard
      t"Hello world".digest[Sha2[512]].serialize[Base64]
    . assert(_ == t"t/eDuu2Cl/DbkXRiGE/08I5pwtXl95qUJgD5cl9Yzh8pwYE5v4CwbA//K900c4RS7PQMSIwip+PYDN9vnBwNRw==")

    test(m"Encode to Binary"):
      import alphabets.binary.standard
      IArray[Byte](1, 2, 3, 4).serialize[Binary]
    . assert(_ == t"00000001000000100000001100000100")

    test(m"Extract PEM message type"):
      val example = t"""
        |-----BEGIN EXAMPLE-----
        |MIIB9TCCAWACAQAwgbgxGTAXBgNVBAoMEFF1b1ZhZGlzIExpbWl0ZWQxHDAaBgNV
        |-----END EXAMPLE-----
        """.s.stripMargin.show

      Pem.parse(example).label
    . assert(_ == PemLabel.Proprietary(t"EXAMPLE"))

    test(m"Decode PEM certificate"):
      import alphabets.base64.standard
      Pem.parse(request).data.digest[Md5].serialize[Base64]
    . assert(_ == t"iMwRdyDFStqq08vqjPbzYw==")

    test(m"PEM roundtrip"):
      Pem.parse(request).serialize
    . assert(_ == request.trim)

    test(m"RSA roundtrip"):
      val privateKey: PrivateKey[Rsa[1024]] = PrivateKey.generate[Rsa[1024]]()
      val message: Data = privateKey.public.expose:
        t"Hello world".encrypt(InitializationVector.random)
      privateKey.expose:
        message.decrypt.text
    . assert(_ == t"Hello world")

    test(m"AES roundtrip"):
      import blockCipherMode.cbc, blockCipherPadding.pkcs7
      val key: SymmetricKey[Aes[256]] = SymmetricKey.generate[Aes[256]]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"AES/CBC/PKCS7 roundtrip (mode and padding fully specified)"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"AES/ECB/ISO10126 roundtrip (mode and padding fully specified)"):
      val key = SymmetricKey.generate[Aes[256] over Ecb against Iso10126]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"AES/CTR/NoPadding roundtrip (mode and padding fully specified)"):
      val key = SymmetricKey.generate[Aes[128] over Ctr against NoPadding]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"AES/CBC with padding inferred as PKCS7 from import"):
      import blockCipherPadding.pkcs7
      val key = SymmetricKey.generate[Aes[256] over Cbc]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"AES with mode and padding inferred as CBC/PKCS7 from imports"):
      import blockCipherMode.cbc, blockCipherPadding.pkcs7
      val key = SymmetricKey.generate[Aes[256]]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"CBC encryption of the same plaintext differs run-to-run (random IV)"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        val first = t"Hello world".encrypt(InitializationVector.random).serialize[Hex]
        val second = t"Hello world".encrypt(InitializationVector.random).serialize[Hex]
        first == second
    . assert(_ == false)

    test(m"A fixed IV makes CBC encryption deterministic"):
      val iv = InitializationVector.fixed(t"0123456789abcdef".data)
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        t"Hello world".encrypt(iv).serialize[Hex] == t"Hello world".encrypt(iv).serialize[Hex]
    . assert(_ == true)

    test(m"A fixed IV still round-trips"):
      val iv = InitializationVector.fixed(t"0123456789abcdef".data)
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        t"Hello world".encrypt(iv).decrypt.text
    . assert(_ == t"Hello world")

    test(m"A zero IV makes CBC encryption deterministic"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        val first = t"Hello world".encrypt(InitializationVector.zero).serialize[Hex]
        val second = t"Hello world".encrypt(InitializationVector.zero).serialize[Hex]
        first == second
    . assert(_ == true)

    test(m"A custom Crypto provider can supply deterministic randomness"):
      // A provider that reuses the JDK's algorithms but fixes its randomness; the
      // resulting all-zero IV makes CBC encryption repeatable, demonstrating that
      // the provider seam (and its random source) is injectable.
      given Crypto:
        def random: Crypto.Random = size => IArray.fill[Byte](size)(0.toByte)
        def aes: Crypto.SymmetricCipher = JavaStdlibCrypto.aes
        def rsa: Crypto.PublicKeyCipher = JavaStdlibCrypto.rsa
        def hmac(algorithm: Text): Crypto.Mac = JavaStdlibCrypto.hmac(algorithm)

      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        val first = t"Hello world".encrypt(InitializationVector.random).serialize[Hex]
        val second = t"Hello world".encrypt(InitializationVector.random).serialize[Hex]
        first == second
    . assert(_ == true)

    test(m"DES/CBC/PKCS7 roundtrip"):
      val key = SymmetricKey.generate[Des over Cbc against Pkcs7]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"TripleDES/CBC/PKCS7 roundtrip"):
      val key = SymmetricKey.generate[TripleDes[168] over Cbc against Pkcs7]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"Blowfish/CBC/PKCS7 roundtrip"):
      val key = SymmetricKey.generate[Blowfish[448] over Cbc against Pkcs7]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"RC2/CFB/ISO10126 roundtrip"):
      val key = SymmetricKey.generate[Rc2[128] over Cfb against Iso10126]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"DES/CTR/NoPadding roundtrip"):
      val key = SymmetricKey.generate[Des over Ctr against NoPadding]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"Decryption with the wrong key fails with a CryptoError"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val wrongKey = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val ciphertext = key.expose(t"Hello world".encrypt(InitializationVector.random))
      capture[CryptoError](wrongKey.expose(ciphertext.decrypt.text)).reason
    . assert(_ == CryptoError.Reason.BadPadding)

    test(m"AES/CBC/NoPadding round-trips block-aligned input"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against NoPadding]()
      key.expose:
        t"0123456789abcdef".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"0123456789abcdef")

    test(m"AES/CBC/NoPadding rejects misaligned input with a CryptoError"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against NoPadding]()
      capture[CryptoError](key.expose(t"Hello world".encrypt(InitializationVector.random))).reason
    . assert(_ == CryptoError.Reason.IllegalBlockSize)

    test(m"AES/CTR/NoPadding (stream mode) accepts any length"):
      val key = SymmetricKey.generate[Aes[256] over Ctr against NoPadding]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.text
    . assert(_ == t"Hello world")

    test(m"Streaming encryption round-trips via one-shot decryption"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        val chunks = Stream(t"Hello, ".data, t"streaming ".data, t"world!".data)
        chunks.encrypt(InitializationVector.random).reduce(_ ++ _).decrypt.text
    . assert(_ == t"Hello, streaming world!")

    test(m"Streaming and one-shot encryption agree for a fixed IV"):
      val iv = InitializationVector.fixed(t"0123456789abcdef".data)
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        val streamed =
          Stream(t"Hello, ".data, t"streaming ".data, t"world!".data).encrypt(iv).reduce(_ ++ _)

        streamed.serialize[Hex] == t"Hello, streaming world!".data.encrypt(iv).serialize[Hex]
    . assert(_ == true)

    test(m"Sign some data with DSA"):
      val privateKey: PrivateKey[Dsa[1024]] = PrivateKey.generate[Dsa[1024]]()
      val message = t"Hello world"
      val signature = privateKey.sign(message)
      privateKey.public.verify(message, signature)
    . assert(identity)

    test(m"Check bad signature"):
      val privateKey: PrivateKey[Dsa[1024]] = PrivateKey.generate[Dsa[1024]]()
      val message = t"Hello world"
      val signature = privateKey.sign(t"Something else")
      privateKey.public.verify(message, signature)
    . assert(!identity(_))

    test(m"MD5 HMAC"):
      pangram.hmac[Md5](t"key".data).serialize[Hex]
    .check(_ == t"80070713463E7749B90C2DC24911E275")

    test(m"SHA1 HMAC"):
      pangram.hmac[Sha1](t"key".data).serialize[Hex]
    . assert(_ == t"DE7C9B85B8B78AA6BC8A7A36F70A90701C9DB4D9")

    test(m"SHA256 HMAC"):
      pangram.hmac[Sha2[256]](t"key".data).serialize[Hex]
    . assert(_ == t"F7BC83F430538424B13298E6AA6FB143EF4D59A14946175997479DBC2D1A3CD8")

    test(m"SHA384 HMAC"):
      import alphabets.base64.standard
      pangram.hmac[Sha2[384]](t"key".data).serialize[Base64]
    . assert(_ == t"1/RyfiwLOa4PHkDMlvYCQtW3gBhBzqb8WSxdPhrlBwBYKpbPNeHlVJlf5OAzgcI3")

    test(m"SHA512 HMAC"):
      import alphabets.base64.standard
      pangram.hmac[Sha2[512]](t"key".data).serialize[Base64]
    . assert(_ == t"tCrwkFe6weLUFwjkipAuCbX/fxKrQopP6GZTxz3SSPuC+UilSfe3kaW0GRXuTR7Dk1NX5OIxclDQNyr6Lr7rOg==")

    suite(m"COSE Mac0 (HMAC-SHA-256)"):
      val keyBytes: Data = t"a-32-byte-key-for-hmac-sha256!!!".data
      val key: SymmetricKey[HmacCipher[Sha2[256]]] = SymmetricKey(keyBytes)
      val payload: Data = pangram.data

      test(m"Cose(payload, key) round-trips through verify[Cose](key)"):
        val cose = Cose(payload, key)
        cose.bytes.verify[Cose](key)
      . assert(identity)

      test(m"Cose envelope is tagged with CBOR tag 17 (Mac0)"):
        val wire = Cose(payload, key).bytes
        wire(0).toInt & 0xFF
      . assert(_ == 0xD1)   // major type 6 (tag) | tag value 17 = 0xC0 | 17 = 0xD1

      test(m"Verification fails with the wrong key"):
        val wire = Cose(payload, key).bytes
        val wrongKey: SymmetricKey[HmacCipher[Sha2[256]]] =
          SymmetricKey(t"this-is-a-different-key-bytes!!!".data)
        wire.verify[Cose](wrongKey)
      . assert(!_)

      test(m"Verification fails after tampering the wire bytes"):
        val wire = Cose(payload, key).bytes
        val tampered = wire.mutable(using Unsafe)
        // Flip a bit in the MAC tag near the end of the envelope.
        tampered(tampered.length - 5) = (tampered(tampered.length - 5) ^ 0xFF.toByte).toByte
        tampered.immutable(using Unsafe).verify[Cose](key)
      . assert(!_)

    suite(m"OpenSSL provider (libcrypto via xenophile FFM)"):
      // A local `given Crypto` outranks the file-level `javaStdlibCrypto` import,
      // so each block unambiguously selects its provider; cross-validating the two
      // proves the OpenSSL path agrees with the JDK on the wire.
      val key32: Data = t"a-32-byte-key-for-aes-256-cbc!!!".data

      test(m"RAND_bytes returns the requested number of bytes"):
        OpensslCrypto.random.bytes(32).length
      . assert(_ == 32)

      test(m"HMAC-SHA256 agrees with the JDK provider"):
        val jdk = pangram.hmac[Sha2[256]](t"key".data).serialize[Hex]
        val openssl = { given Crypto = OpensslCrypto; pangram.hmac[Sha2[256]](t"key".data).serialize[Hex] }
        jdk == openssl
      . assert(_ == true)

      test(m"HMAC-SHA512 agrees with the JDK provider"):
        val jdk = pangram.hmac[Sha2[512]](t"key".data).serialize[Hex]
        val openssl = { given Crypto = OpensslCrypto; pangram.hmac[Sha2[512]](t"key".data).serialize[Hex] }
        jdk == openssl
      . assert(_ == true)

      test(m"AES-256-CBC ciphertext agrees with the JDK provider (fixed IV)"):
        val iv = InitializationVector.fixed(t"0123456789abcdef".data)
        val key: SymmetricKey[Aes[256] over Cbc against Pkcs7] = SymmetricKey(key32)
        val jdk = key.expose(t"Hello world".encrypt(iv).serialize[Hex])

        val openssl =
          given Crypto = OpensslCrypto
          key.expose(t"Hello world".encrypt(iv).serialize[Hex])

        jdk == openssl
      . assert(_ == true)

      test(m"AES-256-CBC round-trips under the OpenSSL provider"):
        given Crypto = OpensslCrypto
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        key.expose(t"Hello world".encrypt(InitializationVector.random).decrypt.text)
      . assert(_ == t"Hello world")

      test(m"OpenSSL decrypts what the JDK encrypted (CBC, fixed IV)"):
        val iv = InitializationVector.fixed(t"0123456789abcdef".data)
        val key: SymmetricKey[Aes[256] over Cbc against Pkcs7] = SymmetricKey(key32)
        val ciphertext = key.expose(t"Interoperable!".encrypt(iv))
        val plaintext = { given Crypto = OpensslCrypto; key.expose(ciphertext.decrypt.text) }
        plaintext
      . assert(_ == t"Interoperable!")

      test(m"AES-128-CTR/NoPadding round-trips under the OpenSSL provider"):
        given Crypto = OpensslCrypto
        val key = SymmetricKey.generate[Aes[128] over Ctr against NoPadding]()
        key.expose(t"Hello world".encrypt(InitializationVector.random).decrypt.text)
      . assert(_ == t"Hello world")

      test(m"OpenSSL streaming encryption round-trips via one-shot decryption"):
        given Crypto = OpensslCrypto
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        key.expose:
          val chunks = Stream(t"Hello, ".data, t"streaming ".data, t"world!".data)
          chunks.encrypt(InitializationVector.random).reduce(_ ++ _).decrypt.text
      . assert(_ == t"Hello, streaming world!")
