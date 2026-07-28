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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import charDecoders.utf8Decoder, charEncoders.utf8Encoder, textSanitizers.skipSanitizer
import gossamer.textDecodable
import errorDiagnostics.stackTracesDiagnostics
import providers.javaStdlibProvider
import crypto.permitDisallowedCrypto   // the suite deliberately exercises weak crypto
import cloaks.cloakHeap

import alphabets.hexUpperCase

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

  // A self-signed certificate, produced by `openssl req -x509 -newkey rsa:2048`. It is here for its
  // shape rather than its content: v3 extensions behind a `[3] EXPLICIT` tag, a `[0] EXPLICIT`
  // version, RDNs in `SET`s, object identifiers, `UTCTime`s and a `BIT STRING` signature.
  val certificate: Text = t"""
    |-----BEGIN CERTIFICATE-----
    |MIIDtTCCAp2gAwIBAgIUfOcoc0ZbdhisC8NxLHW9Nj+CvPEwDQYJKoZIhvcNAQEL
    |BQAwajELMAkGA1UEBhMCR0IxFzAVBgNVBAgMDkNhbWJyaWRnZXNoaXJlMRIwEAYD
    |VQQHDAlDYW1icmlkZ2UxEzARBgNVBAoMClByb3BlbnNpdmUxGTAXBgNVBAMMEGFz
    |bjEuZXhhbXBsZS5jb20wHhcNMjYwNzI4MTEwNzIyWhcNMzYwNzI1MTEwNzIyWjBq
    |MQswCQYDVQQGEwJHQjEXMBUGA1UECAwOQ2FtYnJpZGdlc2hpcmUxEjAQBgNVBAcM
    |CUNhbWJyaWRnZTETMBEGA1UECgwKUHJvcGVuc2l2ZTEZMBcGA1UEAwwQYXNuMS5l
    |eGFtcGxlLmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAN6Yqkcv
    |lMJP+20lvvwabQXqHLM6eqAOcH1eeP74qe8lkGL4MFooAfUU2hifXdPPXvrE21dK
    |kifgk9tOd8SkX+EDY4fGn6MEN5hm5l4nOXsZnqhm1lu1EbWgcDdw22Rd+6kPFC35
    |7JeLL76/6L8XMi1MLNiyuIgozisiGf9TGLPF1tuexPvJZaf/4XrAA7fDdBfz7F5M
    |PjOAJyT468qcSS1kMZ4weL0yH3kQ8mlwgNmyz5ibPF2KNUVpt194Y8GsXOeqQU+C
    |joR/LZ/wMh5Iaq8mWjx/TcL/Yac7hhuIttEO71SMZ5+N0g8BGI+ZagVkkJ4FUziW
    |sluLqCKEiRMrhvECAwEAAaNTMFEwHQYDVR0OBBYEFLMi3Mw9tUXd2rkjBuA96mQk
    |Dk+TMB8GA1UdIwQYMBaAFLMi3Mw9tUXd2rkjBuA96mQkDk+TMA8GA1UdEwEB/wQF
    |MAMBAf8wDQYJKoZIhvcNAQELBQADggEBAJspyiAbb9RMGDAKruzVj7nnbmVSv7Kj
    |shPGW6PWd2uYJvVcpTcGNTepZ61PoEt8zjUUgGXNRSc9SoeY/17I/K9pq79aHkl4
    |TeBwly0NdkyP788FmpOTbICmBADyM3slCr48GCnH0P5fgBQXv8cXm/fYYWUB4c9p
    |h+wOiEQOTAJ1m/9kW7/kF7AkFahywqRXOkRUOAAvWDxVrn+FZFHyH1e0PXQssbq9
    |eeoVq8kR6euQcL4Vyrg9XpOolSlBpJSOnG/S8CP4/JOQxfoXIr1ERuJrLTF+Kniw
    |VWAjJOn3se6f9d+2rhKmV9Z7W21pdb87yuXgWXl/F6c/wVmOallb/Fw=
    |-----END CERTIFICATE-----
    """.s.stripMargin.show

  val pangram: Text = t"The quick brown fox jumps over the lazy dog"

  def run(): Unit =
    test(m"Sha256, Hex"):
      t"Hello world".digest[Sha2[256]].serialize[Hex]
    . assert(_ == t"64EC88CA00B268E5BA1A35678A1B5316D212F4F366B2477232534A8AECA37F3C")

    test(m"Md5, Base64"):
      import alphabets.base64Standard
      t"Hello world".digest[Md5].serialize[Base64]
    . assert(_ == t"PiWWCnnbxptnTNTsZ6csYg==")

    test(m"Sha1, Base64Url"):
      import alphabets.base64Url
      t"Hello world".digest[Sha1].data.serialize[Base64]
    . assert(_ == t"e1AsOh9IyGCa4hLN-2Od7jlnP14")

    test(m"Sha384, Base64"):
      import alphabets.base64Standard
      t"Hello world".digest[Sha2[384]].serialize[Base64]
    . assert(_ == t"kgOwxEOf0eauWHiGYze3xTKs1tkmAVDIAxjoq4wnzjMBifjflPuJDfHSmP82Bifh")

    test(m"Sha512, Base64"):
      import alphabets.base64Standard
      t"Hello world".digest[Sha2[512]].serialize[Base64]
    . assert(_ == t"t/eDuu2Cl/DbkXRiGE/08I5pwtXl95qUJgD5cl9Yzh8pwYE5v4CwbA//K900c4RS7PQMSIwip+PYDN9vnBwNRw==")

    test(m"Encode to Binary"):
      import alphabets.binaryStandard
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
      import alphabets.base64Standard
      Pem.parse(request).data.digest[Md5].serialize[Base64]
    . assert(_ == t"iMwRdyDFStqq08vqjPbzYw==")

    test(m"PEM roundtrip"):
      Pem.parse(request).serialize
    . assert(_ == request.trim)

    test(m"PEM parses from a stream through accept"):
      import alphabets.base64Standard
      val stream = request.s.grouped(7).map(_.tt).stream
      summon[Pem is Aggregable by Text].accept(stream).data.digest[Md5].serialize[Base64]
    . assert(_ == t"iMwRdyDFStqq08vqjPbzYw==")

    test(m"PEM parses from a single-char-chunk stream"):
      val stream = request.s.grouped(1).map(_.tt).stream
      summon[Pem is Aggregable by Text].accept(stream).label
    . assert(_ == PemLabel.CertificateRequest)

    test(m"PEM certificate chain parses lazily from a stream"):
      val example = t"-----BEGIN EXAMPLE-----\nAAAA\n-----END EXAMPLE-----\n"
      val chain = t"subject=/CN=example\n$example\nissuer comment\n$example$example"
      val stream = chain.s.grouped(11).map(_.tt).stream
      summon[LazyList[Pem] is Aggregable by Text].accept(stream).map(_.label).to(List)
    . assert(_ == List.fill(3)(PemLabel.Proprietary(t"EXAMPLE")))

    test(m"PEM chain of an input without blocks is empty"):
      summon[LazyList[Pem] is Aggregable by Text].accept(t"no blocks here\n".stream).to(List)
    . assert(_ == List())

    test(m"PEM streams its armored form"):
      Pem.parse(request).read[Text].trim
    . assert(_ == request.trim)

    test(m"RSA roundtrip"):
      val privateKey: PrivateKey[Rsa[1024]] = PrivateKey.generate[Rsa[1024]]()
      val message: Data = privateKey.public.uncloak:
        t"Hello world".encrypt(InitializationVector.random)
      privateKey.uncloak:
        message.decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"AES roundtrip"):
      import blockCipherMode.cbc, blockCipherPadding.pkcs7
      val key: SymmetricKey[Aes[256]] = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"AES/CBC/PKCS7 roundtrip (mode and padding fully specified)"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"AES/ECB/ISO10126 roundtrip (mode and padding fully specified)"):
      val key = SymmetricKey.generate[Aes[256] over Ecb against Iso10126]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"AES/CTR/NoPadding roundtrip (mode and padding fully specified)"):
      val key = SymmetricKey.generate[Aes[128] over Ctr against NoPadding]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"AES/CBC with padding inferred as PKCS7 from import"):
      import blockCipherPadding.pkcs7
      val key = SymmetricKey.generate[Aes[256] over Cbc]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"AES with mode and padding inferred as CBC/PKCS7 from imports"):
      import blockCipherMode.cbc, blockCipherPadding.pkcs7
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"stream-encrypted data decrypts through the whole-value path"):
      import blockCipherMode.cbc, blockCipherPadding.pkcs7
      import charEncoders.utf8Encoder
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        t"Hello world".in[Data].stream.encrypt(InitializationVector.random).memoize
        . decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"whole-value-encrypted data decrypts through a stream"):
      import blockCipherMode.cbc, blockCipherPadding.pkcs7
      import charEncoders.utf8Encoder
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).stream.decrypt.memoize.to(List)
    . assert(_ == t"Hello world".in[Data].to(List))

    test(m"one-byte-chunk streams roundtrip through stream encrypt and decrypt"):
      import blockCipherMode.cbc, blockCipherPadding.pkcs7
      import charEncoders.utf8Encoder
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        val plain = t"The quick brown fox jumps over the lazy dog".in[Data]
        val encrypted = plain.grouped(1).iterator.stream.encrypt(InitializationVector.random)
        encrypted.memoize.grouped(1).iterator.stream.decrypt.memoize.to(List)
    . assert(_ == t"The quick brown fox jumps over the lazy dog".in[Data].to(List))

    test(m"CTR/NoPadding streams roundtrip (stream-aligned check at end)"):
      import charEncoders.utf8Encoder
      val key = SymmetricKey.generate[Aes[128] over Ctr against NoPadding]()
      key.uncloak:
        t"Hello world".in[Data].stream.encrypt(InitializationVector.random).memoize
        . stream.decrypt.memoize.to(List)
    . assert(_ == t"Hello world".in[Data].to(List))

    test(m"legacy LazyList encryption survives one-byte chunks"):
      import blockCipherMode.cbc, blockCipherPadding.pkcs7
      import charEncoders.utf8Encoder
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        val plain = t"Hello world".in[Data]
        val chunks = plain.grouped(1).map { chunk => chunk }.to(LazyList)
        chunks.encrypt(InitializationVector.random).reduce(_ ++ _).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"CBC encryption of the same plaintext differs run-to-run (random IV)"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        val first = t"Hello world".encrypt(InitializationVector.random).serialize[Hex]
        val second = t"Hello world".encrypt(InitializationVector.random).serialize[Hex]
        first == second
    . assert(_ == false)

    test(m"A fixed IV makes CBC encryption deterministic"):
      val iv = InitializationVector.fixed(t"0123456789abcdef".in[Data])
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        t"Hello world".encrypt(iv).serialize[Hex] == t"Hello world".encrypt(iv).serialize[Hex]
    . assert(_ == true)

    test(m"A fixed IV still round-trips"):
      val iv = InitializationVector.fixed(t"0123456789abcdef".in[Data])
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        t"Hello world".encrypt(iv).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"A zero IV makes CBC encryption deterministic"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
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
      key.uncloak:
        val first = t"Hello world".encrypt(InitializationVector.random).serialize[Hex]
        val second = t"Hello world".encrypt(InitializationVector.random).serialize[Hex]
        first == second
    . assert(_ == true)

    test(m"DES/CBC/PKCS7 roundtrip"):
      val key = SymmetricKey.generate[Des over Cbc against Pkcs7]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"TripleDES/CBC/PKCS7 roundtrip"):
      val key = SymmetricKey.generate[TripleDes[168] over Cbc against Pkcs7]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"Blowfish/CBC/PKCS7 roundtrip"):
      val key = SymmetricKey.generate[Blowfish[448] over Cbc against Pkcs7]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"RC2/CFB/ISO10126 roundtrip"):
      val key = SymmetricKey.generate[Rc2[128] over Cfb against Iso10126]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"DES/CTR/NoPadding roundtrip"):
      val key = SymmetricKey.generate[Des over Ctr against NoPadding]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"Decryption with the wrong key fails with a CryptoError"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val wrongKey = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val ciphertext = key.uncloak(t"Hello world".encrypt(InitializationVector.random))
      capture[CryptoError](wrongKey.uncloak(ciphertext.decrypt.as[Text])).reason
    . assert(_ == CryptoError.Reason.BadPadding)

    test(m"Streaming decryption with the wrong key raises a CryptoError"):
      // The provider's tag/padding failure surfaces at end-of-stream as a typed
      // `CryptoError`, not the raw JCE exception, when the final window is pulled.
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val wrongKey = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val ciphertext = key.uncloak(t"Hello world".encrypt(InitializationVector.random))
      capture[CryptoError](wrongKey.uncloak(ciphertext.stream.decrypt.memoize)).reason
    . assert(_ == CryptoError.Reason.BadPadding)

    test(m"AES/CBC/NoPadding round-trips block-aligned input"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against NoPadding]()
      key.uncloak:
        t"0123456789abcdef".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"0123456789abcdef")

    test(m"AES/CBC/NoPadding rejects misaligned input with a CryptoError"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against NoPadding]()
      capture[CryptoError](key.uncloak(t"Hello world".encrypt(InitializationVector.random))).reason
    . assert(_ == CryptoError.Reason.IllegalBlockSize)

    test(m"AES/CTR/NoPadding (stream mode) accepts any length"):
      val key = SymmetricKey.generate[Aes[256] over Ctr against NoPadding]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"Streaming encryption round-trips via one-shot decryption"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        val chunks = LazyList(t"Hello, ".in[Data], t"streaming ".in[Data], t"world!".in[Data])
        chunks.encrypt(InitializationVector.random).reduce(_ ++ _).decrypt.as[Text]
    . assert(_ == t"Hello, streaming world!")

    test(m"Streaming and one-shot encryption agree for a fixed IV"):
      val iv = InitializationVector.fixed(t"0123456789abcdef".in[Data])
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        val streamed =
          LazyList(t"Hello, ".in[Data], t"streaming ".in[Data], t"world!".in[Data]).encrypt(iv).reduce(_ ++ _)

        streamed.serialize[Hex] == t"Hello, streaming world!".in[Data].encrypt(iv).serialize[Hex]
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
      pangram.hmac[Md5](t"key".in[Data]).serialize[Hex]
    .check(_ == t"80070713463E7749B90C2DC24911E275")

    test(m"SHA1 HMAC"):
      pangram.hmac[Sha1](t"key".in[Data]).serialize[Hex]
    . assert(_ == t"DE7C9B85B8B78AA6BC8A7A36F70A90701C9DB4D9")

    test(m"SHA256 HMAC"):
      pangram.hmac[Sha2[256]](t"key".in[Data]).serialize[Hex]
    . assert(_ == t"F7BC83F430538424B13298E6AA6FB143EF4D59A14946175997479DBC2D1A3CD8")

    test(m"SHA384 HMAC"):
      import alphabets.base64Standard
      pangram.hmac[Sha2[384]](t"key".in[Data]).serialize[Base64]
    . assert(_ == t"1/RyfiwLOa4PHkDMlvYCQtW3gBhBzqb8WSxdPhrlBwBYKpbPNeHlVJlf5OAzgcI3")

    test(m"SHA512 HMAC"):
      import alphabets.base64Standard
      pangram.hmac[Sha2[512]](t"key".in[Data]).serialize[Base64]
    . assert(_ == t"tCrwkFe6weLUFwjkipAuCbX/fxKrQopP6GZTxz3SSPuC+UilSfe3kaW0GRXuTR7Dk1NX5OIxclDQNyr6Lr7rOg==")

    suite(m"COSE Mac0 (HMAC-SHA-256)"):
      val keyBytes: Data = t"a-32-byte-key-for-hmac-sha256!!!".in[Data]
      val key: SymmetricKey[HmacCipher[Sha2[256]]] = SymmetricKey(keyBytes)
      val payload: Data = pangram.in[Data]

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
          SymmetricKey(t"this-is-a-different-key-bytes!!!".in[Data])
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
      val key32: Data = t"a-32-byte-key-for-aes-256-cbc!!!".in[Data]

      test(m"RAND_bytes returns the requested number of bytes"):
        OpensslCrypto.random.bytes(32).length
      . assert(_ == 32)

      test(m"HMAC-SHA256 agrees with the JDK provider"):
        val jdk = pangram.hmac[Sha2[256]](t"key".in[Data]).serialize[Hex]
        val openssl = { given Crypto = OpensslCrypto; pangram.hmac[Sha2[256]](t"key".in[Data]).serialize[Hex] }
        jdk == openssl
      . assert(_ == true)

      test(m"HMAC-SHA512 agrees with the JDK provider"):
        val jdk = pangram.hmac[Sha2[512]](t"key".in[Data]).serialize[Hex]
        val openssl = { given Crypto = OpensslCrypto; pangram.hmac[Sha2[512]](t"key".in[Data]).serialize[Hex] }
        jdk == openssl
      . assert(_ == true)

      test(m"AES-256-CBC ciphertext agrees with the JDK provider (fixed IV)"):
        val iv = InitializationVector.fixed(t"0123456789abcdef".in[Data])
        val key: SymmetricKey[Aes[256] over Cbc against Pkcs7] = SymmetricKey(key32)
        val jdk = key.uncloak(t"Hello world".encrypt(iv).serialize[Hex])

        val openssl =
          given Crypto = OpensslCrypto
          key.uncloak(t"Hello world".encrypt(iv).serialize[Hex])

        jdk == openssl
      . assert(_ == true)

      test(m"AES-256-CBC round-trips under the OpenSSL provider"):
        given Crypto = OpensslCrypto
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        key.uncloak(t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text])
      . assert(_ == t"Hello world")

      test(m"OpenSSL decrypts what the JDK encrypted (CBC, fixed IV)"):
        val iv = InitializationVector.fixed(t"0123456789abcdef".in[Data])
        val key: SymmetricKey[Aes[256] over Cbc against Pkcs7] = SymmetricKey(key32)
        val ciphertext = key.uncloak(t"Interoperable!".encrypt(iv))
        val plaintext = { given Crypto = OpensslCrypto; key.uncloak(ciphertext.decrypt.as[Text]) }
        plaintext
      . assert(_ == t"Interoperable!")

      test(m"AES-128-CTR/NoPadding round-trips under the OpenSSL provider"):
        given Crypto = OpensslCrypto
        val key = SymmetricKey.generate[Aes[128] over Ctr against NoPadding]()
        key.uncloak(t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text])
      . assert(_ == t"Hello world")

      test(m"OpenSSL streaming encryption round-trips via one-shot decryption"):
        given Crypto = OpensslCrypto
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        key.uncloak:
          val chunks = LazyList(t"Hello, ".in[Data], t"streaming ".in[Data], t"world!".in[Data])
          chunks.encrypt(InitializationVector.random).reduce(_ ++ _).decrypt.as[Text]
      . assert(_ == t"Hello, streaming world!")

    suite(m"Keystores"):
      import java.io as ji
      import java.security as js

      given (Text is Abstractable across Paths to Text) = identity(_)

      def createKeystore(password: Array[Char] | Null): Text =
        val path = t"/tmp/enigmatic-keystore-${java.util.UUID.randomUUID.nn.toString}.p12"
        val keystore = js.KeyStore.getInstance("PKCS12").nn
        keystore.load(null, null)
        val out = ji.FileOutputStream(path.s)
        try keystore.store(out, if password == null then Array.empty[Char] else password)
        finally out.close()
        path

      val guarded = createKeystore(Array('s', 'e', 's', 'a', 'm', 'e'))

      test(m"An empty keystore opens with the right password"):
        guarded.open[Keystore](Password(t"sesame")):
          keystore.aliases
      . assert(_ == Nil)

      test(m"A wrong password is refused as Unreadable"):
        capture[KeystoreError](guarded.open[Keystore](Password(t"wrong")) { () }).reason
      . assert(_ == KeystoreError.Reason.Unreadable)

      test(m"Opening a keystore for writing is refused"):
        capture[KeystoreError](guarded.open[Keystore](Write, Password(t"sesame")) { () }).reason
      . assert(_ == KeystoreError.Reason.WriteUnsupported)

      test(m"A missing certificate alias is Unset"):
        guarded.open[Keystore](Password(t"sesame")):
          keystore.certificate(t"absent")
      . assert(_ == Unset)

      test(m"A keystore opens with a password held by the veiled-heap cloak"):
        import soundness.cloaks.cloakVeiledHeap
        guarded.open[Keystore](Password(t"sesame")):
          keystore.aliases
      . assert(_ == Nil)

    suite(m"Cloak strategies"):
      def roundtrip()(using cloak: Cloak): Text =
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        key.uncloak(t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text])

      def passwordTrip()(using cloak: Cloak): Text =
        Password(t"hunter2").uncloak(String(cleartext.chars).tt)

      // Each strategy is imported through the `soundness` bundle, exercising the re-export
      // of the (inline, capability-yielding) givens as well as the strategies themselves.
      test(m"AES round-trips through the heap cloak"):
        import soundness.cloaks.cloakHeap
        roundtrip()
      . assert(_ == t"Hello world")

      test(m"AES round-trips through the off-heap cloak"):
        import soundness.cloaks.cloakOffHeap
        roundtrip()
      . assert(_ == t"Hello world")

      test(m"AES round-trips through the veiled-heap cloak"):
        import soundness.cloaks.cloakVeiledHeap
        roundtrip()
      . assert(_ == t"Hello world")

      test(m"AES round-trips through the veiled-off-heap cloak"):
        import soundness.cloaks.cloakVeiledOffHeap
        roundtrip()
      . assert(_ == t"Hello world")

      test(m"a password round-trips through the off-heap cloak"):
        import soundness.cloaks.cloakOffHeap
        passwordTrip()
      . assert(_ == t"hunter2")

      test(m"a password round-trips through the veiled-heap cloak"):
        import soundness.cloaks.cloakVeiledHeap
        passwordTrip()
      . assert(_ == t"hunter2")

      test(m"a password round-trips through the veiled-off-heap cloak"):
        import soundness.cloaks.cloakVeiledOffHeap
        passwordTrip()
      . assert(_ == t"hunter2")

      test(m"constructing a password from chars zeroes the input array"):
        val chars = Array('h', 'u', 'n', 't', 'e', 'r', '2')
        val password = Password(chars)
        (chars.forall(_ == '\u0000'), password.uncloak(String(cleartext.chars).tt))
      . assert(_ == (true, t"hunter2"))

      test(m"DSA signing works through an off-heap cloak"):
        import soundness.cloaks.cloakOffHeap
        val key = PrivateKey.generate[Dsa[1024]]()
        val signature = key.sign(t"attested")
        key.public.verify(t"attested", signature)
      . assert(_ == true)

      test(m"a symmetric key's material survives multiple uncloaks intact"):
        import soundness.cloaks.cloakVeiledOffHeap
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        val first = key.data(Divulgence)
        val second = key.data(Divulgence)
        first.serialize[Hex] == second.serialize[Hex]
      . assert(_ == true)

    suite(m"ASN.1 and DER"):
      // DER is canonical, so `encode` is the equality of record: the byte-carrying cases of `Asn1`
      // are case classes over `IArray[Byte]`, whose synthesized `equals` compares arrays by
      // identity. Every assertion below therefore compares hexadecimal encodings.
      def der(value: Asn1): Text = value.in[Der].data.serialize[Hex]
      def decode(hex: Text): Asn1 = Der(hex.deserialize[Hex]).as[Asn1]
      def roundtrip(hex: Text): Text = hex.deserialize[Hex].read[Der].as[Asn1].in[Der].data.serialize[Hex]
      def reason(hex: Text): Asn1Error.Reason = capture[Asn1Error](decode(hex)).reason

      // Every vector below was generated with `openssl asn1parse -genstr`/`-genconf`, except the
      // `BIT STRING`, which is X.690 §8.6.4.2's worked example.
      val vectors: List[Text] =
        List
         (t"0500", t"0101FF", t"010100", t"020100", t"02017F", t"02020080", t"0201FF", t"020180",
          t"0202FF7F", t"02020100", t"04050102030405", t"0304066E5DC0", t"030100",
          t"06092A864886F70D01010B", t"0603550403", t"06022A03", t"0603813403",
          t"0C0668C3A96C6C6F", t"130454657374", t"16076140622E636F6D",
          t"170D3730303130313030303030305A", t"180F31393730303130313030303030305A",
          t"30050201010500", t"3000", t"3106040101040102", t"310702010102020100", t"A003020102",
          t"8001AB", t"BF1F020500", t"A30405000500")

      test(m"Null, booleans and empty sequences encode as their tag and length"):
        List(der(Asn1.Null), der(Asn1.Boolean(true)), der(Asn1.Boolean(false)),
            der(Asn1.Sequence(Nil)))
      . assert(_ == List(t"0500", t"0101FF", t"010100", t"3000"))

      test(m"Integers encode minimally, with sign extension"):
        List(0, 127, 128, -1, -128, -129, 256).map(value => der(Asn1.Integer(BigInt(value))))
      . assert(_ == List(t"020100", t"02017F", t"02020080", t"0201FF", t"020180", t"0202FF7F",
          t"02020100"))

      test(m"Octet strings and bit strings encode their content"):
        List(der(Asn1.OctetString(IArray[Byte](1, 2, 3, 4, 5))),
            der(Asn1.BitString(IArray[Byte](0x6e, 0x5d, 0xc0.toByte), 6)),
            der(Asn1.BitString(IArray[Byte](), 0)))
      . assert(_ == List(t"04050102030405", t"0304066E5DC0", t"030100"))

      test(m"Object identifiers combine their first two arcs"):
        List(List(1, 2, 840, 113549, 1, 1, 11), List(2, 5, 4, 3), List(1, 2, 3), List(2, 100, 3))
        . map(arcs => der(Asn1.ObjectId(arcs)))
      . assert(_ == List(t"06092A864886F70D01010B", t"0603550403", t"06022A03", t"0603813403"))

      test(m"Strings encode as UTF-8"):
        List(der(Asn1.Utf8String(t"héllo")), der(Asn1.PrintableString(t"Test")),
            der(Asn1.Ia5String(t"a@b.com")))
      . assert(_ == List(t"0C0668C3A96C6C6F", t"130454657374", t"16076140622E636F6D"))

      test(m"Times encode in DER's single permitted form"):
        List(der(Asn1.UtcTime(0)), der(Asn1.GeneralizedTime(0)))
      . assert(_ == List(t"170D3730303130313030303030305A",
          t"180F31393730303130313030303030305A"))

      test(m"A set's members are sorted by their encodings"):
        val two = Asn1.OctetString(IArray[Byte](2.toByte))
        val one = Asn1.OctetString(IArray[Byte](1.toByte))

        List(der(Asn1.Set(List(two, one))),
            der(Asn1.Set(List(Asn1.Integer(BigInt(256)), Asn1.Integer(BigInt(1))))))
      . assert(_ == List(t"3106040101040102", t"310702010102020100"))

      test(m"Context tags encode explicitly, implicitly and in high-tag form"):
        List(der(Asn1.Tagged(0, true, Asn1.Integer(BigInt(2)))),
            der(Asn1.Tagged(0, false, Asn1.OctetString(IArray[Byte](0xab.toByte)))),
            der(Asn1.Tagged(31, true, Asn1.Null)))
      . assert(_ == List(t"A003020102", t"8001AB", t"BF1F020500"))

      test(m"A long-form length uses the fewest possible bytes"):
        val short = der(Asn1.OctetString(Data.fill(200)(_ => 0)))
        val long = der(Asn1.OctetString(Data.fill(300)(_ => 0)))
        List(short.s.take(6).tt, long.s.take(8).tt)
      . assert(_ == List(t"0481C8", t"0482012C"))

      test(m"Every known-answer vector round-trips byte-exactly"):
        vectors.map(roundtrip(_))
      . assert(_ == vectors)

      test(m"A value reads directly from bytes tagged with its form"):
        t"A003020102".deserialize[Hex].read[Asn1 in Der]
      . assert(_ == Asn1.Tagged(0, true, Asn1.Integer(BigInt(2))))

      test(m"A constructed context tag holding one value is an explicit tag"):
        decode(t"A003020102")
      . assert(_ == Asn1.Tagged(0, true, Asn1.Integer(BigInt(2))))

      test(m"A constructed context tag holding two values is opaque"):
        decode(t"A30405000500") match
          case Asn1.Unknown(2, 3, true, _) => true
          case _                           => false
      . assert(identity)

      test(m"A primitive context tag is opaque"):
        decode(t"8001AB") match
          case Asn1.Unknown(2, 0, false, _) => true
          case _                            => false
      . assert(identity)

      test(m"An unmodelled universal tag is preserved verbatim"):
        roundtrip(t"1E0400480049")
      . assert(_ == t"1E0400480049")

      test(m"Indefinite lengths are rejected"):
        reason(t"0480")
      . assert(_ == Asn1Error.Reason.IndefiniteLength(1))

      test(m"Overlong lengths and tags are rejected"):
        List(reason(t"018101FF"), reason(t"1F1E00"))
      . assert(_ == List(Asn1Error.Reason.NonMinimalLength(1), Asn1Error.Reason.NonMinimalTag(1)))

      test(m"A length needing more than four bytes is rejected"):
        reason(t"048501010101010101")
      . assert(_ == Asn1Error.Reason.Overflow(1))

      test(m"Non-minimal and empty integers are rejected"):
        List(reason(t"02020001"), reason(t"0200"))
      . assert(_ == List(Asn1Error.Reason.NonMinimalInteger(0),
          Asn1Error.Reason.EmptyInteger(0)))

      test(m"A boolean other than 0x00 or 0xFF is rejected"):
        reason(t"010101")
      . assert(_ == Asn1Error.Reason.BadBoolean(0, 1))

      test(m"A null with content is rejected"):
        reason(t"050100")
      . assert(_ == Asn1Error.Reason.BadLength(0, 5, 1))

      test(m"Invalid bit-string padding is rejected"):
        List(reason(t"03020801"), reason(t"03020103"))
      . assert(_ == List(Asn1Error.Reason.BadUnusedBits(0, 8),
          Asn1Error.Reason.BadUnusedBits(0, 1)))

      test(m"Constructed strings and primitive sequences are rejected"):
        List(reason(t"240404020102"), reason(t"1003020101"))
      . assert(_ == List(Asn1Error.Reason.NotPrimitive(0, 4),
          Asn1Error.Reason.NotConstructed(0, 16)))

      test(m"Malformed object identifiers are rejected"):
        List(reason(t"06022A80"), reason(t"06062A8FFFFFFF7F"))
      . assert(_ == List(Asn1Error.Reason.BadOid(3), Asn1Error.Reason.OidArcOverflow(3)))

      test(m"An unsorted set is rejected"):
        reason(t"3106040102040101")
      . assert(_ == Asn1Error.Reason.UnsortedSet(5))

      test(m"An impossible date is rejected"):
        reason(t"170D3730303233303030303030305A")
      . assert(_ == Asn1Error.Reason.BadTime(0))

      test(m"Truncated input and trailing bytes are rejected"):
        List(reason(t"0101"), reason(t"05000500"))
      . assert(_ == List(Asn1Error.Reason.Truncated(2), Asn1Error.Reason.Trailing(2)))

      test(m"The reserved tag number zero is rejected"):
        reason(t"0000")
      . assert(_ == Asn1Error.Reason.ReservedTag(0))

      test(m"A real X.509 certificate round-trips byte-exactly"):
        val pem = Pem.parse(certificate)
        pem.der.as[Asn1].in[Der] == pem.der
      . assert(identity)

      test(m"A real PKCS#10 request round-trips byte-exactly"):
        val pem = Pem.parse(request)
        pem.der.as[Asn1].in[Der] == pem.der
      . assert(identity)

      test(m"A DER document re-armors as PEM"):
        val pem = Pem.parse(certificate)
        Pem(PemLabel.Certificate, pem.der.as[Asn1].in[Der]).serialize
      . assert(_ == certificate.trim)

    CaptureTests()
