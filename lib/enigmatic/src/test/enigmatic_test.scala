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
import charsets.utf8Charset, codepages.utf8Codepage, textSanitizers.skipSanitizer
import gossamer.textDecodable
import errorDiagnostics.stackTracesDiagnostics
import providers.javaBaseProvider
import cryptoPermits.permitDisallowedCrypto   // the suite deliberately exercises weak crypto
import cloaks.heapCloak
import dysasymptotics.linearSize

import alphabets.hexUpperCase

object Tests extends Suite("crypto", m"Enigmatic tests"):

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
      Array[Byte](1, 2, 3, 4).serialize[Binary]
    . assert(_ == t"00000001000000100000001100000100")

    test(m"Extract PEM message type"):
      val example = t"""
        |-----BEGIN EXAMPLE-----
        |MIIB9TCCAWACAQAwgbgxGTAXBgNVBAoMEFF1b1ZhZGlzIExpbWl0ZWQxHDAaBgNV
        |-----END EXAMPLE-----
        """.s.stripMargin.show

      example.read[Pem].label
    . assert(_ == Pem.Label.Proprietary(t"EXAMPLE"))

    test(m"Decode PEM certificate"):
      import alphabets.base64Standard
      request.read[Pem].data.digest[Md5].serialize[Base64]
    . assert(_ == t"iMwRdyDFStqq08vqjPbzYw==")

    test(m"PEM roundtrip"):
      request.read[Pem].serialize
    . assert(_ == request.trim)

    test(m"PEM parses from a stream through accept"):
      import alphabets.base64Standard
      val stream = request.s.grouped(7).map(_.tt).stream
      summon[Pem is Aggregable by Text].accept(stream).data.digest[Md5].serialize[Base64]
    . assert(_ == t"iMwRdyDFStqq08vqjPbzYw==")

    test(m"PEM parses from a single-char-chunk stream"):
      val stream = request.s.grouped(1).map(_.tt).stream
      summon[Pem is Aggregable by Text].accept(stream).label
    . assert(_ == Pem.Label.CertificateRequest)

    test(m"PEM certificate chain parses lazily from a stream"):
      val example = t"-----BEGIN EXAMPLE-----\nAAAA\n-----END EXAMPLE-----\n"
      val chain = t"subject=/CN=example\n$example\nissuer comment\n$example$example"
      val stream = chain.s.grouped(11).map(_.tt).stream
      summon[Chain[Pem] is Aggregable by Text].accept(stream).map(_.label).to[List]
    . assert(_ == List.fill(3)(Pem.Label.Proprietary(t"EXAMPLE")))

    test(m"PEM chain of an input without blocks is empty"):
      summon[Chain[Pem] is Aggregable by Text].accept(t"no blocks here\n".stream).to[List]
    . assert(_ == List())

    test(m"PEM streams its armored form"):
      request.read[Pem].read[Text].trim
    . assert(_ == request.trim)

    test(m"RSA roundtrip"):
      val privateKey: PrivateKey[Rsa[1024]] = PrivateKey.generate[Rsa[1024]]()
      val message: Data = privateKey.public.uncloak:
        t"Hello world".encrypt(InitializationVector.random)
      privateKey.uncloak:
        message.decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"AES roundtrip"):
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
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
      import blockCipherPaddings.pkcs7
      val key = SymmetricKey.generate[Aes[256] over Cbc]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"AES with mode and padding inferred as CBC/PKCS7 from imports"):
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"stream-encrypted data decrypts through the whole-value path"):
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import codepages.utf8Codepage
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        t"Hello world".in[Data].stream.encrypt(InitializationVector.random).memoize
        . decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"whole-value-encrypted data decrypts through a stream"):
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import codepages.utf8Codepage
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).stream.decrypt.memoize.to[List]
    . assert(_ == t"Hello world".in[Data].to[List])

    test(m"one-byte-chunk streams roundtrip through stream encrypt and decrypt"):
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import codepages.utf8Codepage
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        val plain = t"The quick brown fox jumps over the lazy dog".in[Data]
        val encrypted = Stream(plain.batched(1).to[Chain]).encrypt(InitializationVector.random)
        Stream(encrypted.memoize.batched(1).to[Chain]).decrypt.memoize.to[List]
    . assert(_ == t"The quick brown fox jumps over the lazy dog".in[Data].to[List])

    test(m"CTR/NoPadding streams roundtrip (stream-aligned check at end)"):
      import codepages.utf8Codepage
      val key = SymmetricKey.generate[Aes[128] over Ctr against NoPadding]()
      key.uncloak:
        t"Hello world".in[Data].stream.encrypt(InitializationVector.random).memoize
        . stream.decrypt.memoize.to[List]
    . assert(_ == t"Hello world".in[Data].to[List])

    test(m"legacy Chain encryption survives one-byte chunks"):
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import codepages.utf8Codepage
      val key = SymmetricKey.generate[Aes[256]]()
      key.uncloak:
        val plain = t"Hello world".in[Data]
        val chunks = plain.batched(1).to[Chain]
        chunks.encrypt(InitializationVector.random).fold(Array.empty[Byte])(_ + _).decrypt.as[Text]
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
        def random: Crypto.Random = size => Array.fill[Byte](size)(0.toByte)
        def aes: Crypto.SymmetricCipher = JavaBaseCrypto.aes
        def rsa: Crypto.PublicKeyCipher = JavaBaseCrypto.rsa
        def hmac(algorithm: Text): Crypto.Mac = JavaBaseCrypto.hmac(algorithm)

        def rsaSignature(digest: Text): Crypto.SignatureScheme =
          JavaBaseCrypto.rsaSignature(digest)

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

    // Wrong-key CBC decryption yields uniform garbage, and PKCS7 unpadding accepts any
    // final block ending `0x01` (or `0x02 0x02`, and so on), so about one run in 256 the
    // garbage forms valid padding and decryption "succeeds". Both outcomes demonstrate
    // the property that matters, so both are accepted; `Invalid PKCS7 padding is reported
    // as BadPadding` below covers the padding failure deterministically.
    test(m"Decryption with the wrong key never recovers the plaintext"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val wrongKey = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val ciphertext = key.uncloak(t"Hello world".encrypt(InitializationVector.random))

      attempt[Crypto.Error](wrongKey.uncloak(ciphertext.decrypt.as[Text])) match
        case Attempt.Failure(error) => error.reason == Crypto.Error.Reason.BadPadding
        case Attempt.Success(text)  => text != t"Hello world"

    . assert(_ == true)

    test(m"Streaming decryption with the wrong key never recovers the plaintext"):
      // The provider's tag/padding failure surfaces at end-of-stream as a typed
      // `Crypto.Error`, not the raw JCE exception, when the final window is pulled.
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val wrongKey = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      val ciphertext = key.uncloak(t"Hello world".encrypt(InitializationVector.random))

      attempt[Crypto.Error](wrongKey.uncloak(ciphertext.stream.decrypt.memoize)) match
        case Attempt.Failure(error) => error.reason == Crypto.Error.Reason.BadPadding

        case Attempt.Success(data) =>
          data.serialize[Hex] != t"Hello world".in[Data].serialize[Hex]

    . assert(_ == true)

    test(m"AES/CBC/NoPadding round-trips block-aligned input"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against NoPadding]()
      key.uncloak:
        t"0123456789abcdef".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"0123456789abcdef")

    test(m"AES/CBC/NoPadding rejects misaligned input with a Crypto.Error"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against NoPadding]()
      capture[Crypto.Error](key.uncloak(t"Hello world".encrypt(InitializationVector.random))).reason
    . assert(_ == Crypto.Error.Reason.IllegalBlockSize)

    test(m"Invalid PKCS7 padding is reported as BadPadding"):
      val bytes: Data = t"a-32-byte-key-for-aes-256-cbc!!!".in[Data]
      val rawKey: SymmetricKey[Aes[256] over Cbc against NoPadding] = SymmetricKey(bytes)
      val paddedKey: SymmetricKey[Aes[256] over Cbc against Pkcs7] = SymmetricKey(bytes)

      // The recovered final block ends in `f` (0x66), which is never a valid PKCS7 pad
      // length, so unpadding fails for every run rather than for 255 runs in 256.
      val ciphertext = rawKey.uncloak(t"0123456789abcdef".encrypt(InitializationVector.random))
      capture[Crypto.Error](paddedKey.uncloak(ciphertext.decrypt.as[Text])).reason
    . assert(_ == Crypto.Error.Reason.BadPadding)

    test(m"AES/CTR/NoPadding (stream mode) accepts any length"):
      val key = SymmetricKey.generate[Aes[256] over Ctr against NoPadding]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"Streaming encryption round-trips via one-shot decryption"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        val chunks = Chain(t"Hello, ".in[Data], t"streaming ".in[Data], t"world!".in[Data])
        chunks.encrypt(InitializationVector.random).fold(Array.empty[Byte])(_ + _).decrypt.as[Text]
    . assert(_ == t"Hello, streaming world!")

    test(m"Streaming and one-shot encryption agree for a fixed IV"):
      val iv = InitializationVector.fixed(t"0123456789abcdef".in[Data])
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        val streamed =
          Chain(t"Hello, ".in[Data], t"streaming ".in[Data], t"world!".in[Data]).encrypt(iv).fold(Array.empty[Byte])(_ + _)

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

      test(m"Cose(payload, key) round-trips through authenticate[Cose](key)"):
        val cose = Cose(payload, key)
        cose.in[Data].authenticate[Cose](key)
      . assert(identity)

      test(m"Cose envelope is tagged with CBOR tag 17 (Mac0)"):
        val wire = Cose(payload, key).in[Data]
        wire.at(Prim).let(_.toInt & 0xFF)
      . assert(_ == 0xD1)   // major type 6 (tag) | tag value 17 = 0xC0 | 17 = 0xD1

      test(m"Verification fails with the wrong key"):
        val wire = Cose(payload, key).in[Data]
        val wrongKey: SymmetricKey[HmacCipher[Sha2[256]]] =
          SymmetricKey(t"this-is-a-different-key-bytes!!!".in[Data])
        wire.authenticate[Cose](wrongKey)
      . assert(!_)

      test(m"Verification fails after tampering the wire bytes"):
        val wire = Cose(payload, key).in[Data]
        val tampered = Array.allocate[Byte](wire.length)
        tampered.place(wire)
        // Flip a bit in the MAC tag near the end of the envelope.
        val index = wire.length - 5
        tampered(index) = (tampered.readUnchecked(index) ^ 0xFF.toByte).toByte // index is wire.length - 5
        Array.freeze(tampered).authenticate[Cose](key)
      . assert(!_)

    suite(m"OpenSSL provider (libcrypto via xenophile FFM)"):
      // A local `given Crypto` outranks the file-level `javaBaseCrypto` import,
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
          val chunks = Chain(t"Hello, ".in[Data], t"streaming ".in[Data], t"world!".in[Data])
          chunks.encrypt(InitializationVector.random).fold(Array.empty[Byte])(_ + _).decrypt.as[Text]
      . assert(_ == t"Hello, streaming world!")

    suite(m"Keystores"):
      import java.io as ji
      import java.security as js

      given (Text is Abstractable across Paths to Text) = identity(_)

      def createKeystore(password: scala.Array[Char] | Null): Text =
        val path = t"/tmp/enigmatic-keystore-${java.util.UUID.randomUUID.nn.toString}.p12"
        val keystore = js.KeyStore.getInstance("PKCS12").nn
        keystore.load(null, null)
        val out = ji.FileOutputStream(path.s)
        try keystore.store(out, if password == null then scala.Array.empty[Char] else password)
        finally out.close()
        path

      val guarded = createKeystore(scala.Array('s', 'e', 's', 'a', 'm', 'e'))

      test(m"An empty keystore opens with the right password"):
        guarded.open[Keystore](Password(t"sesame")):
          keystore.aliases
      . assert(_ == Nil)

      test(m"A wrong password is refused as Unreadable"):
        capture[Keystore.Error](guarded.open[Keystore](Password(t"wrong")) { () }).reason
      . assert(_ == Keystore.Error.Reason.Unreadable)

      test(m"Opening a keystore for writing is refused"):
        capture[Keystore.Error](guarded.open[Keystore](Write, Password(t"sesame")) { () }).reason
      . assert(_ == Keystore.Error.Reason.WriteUnsupported)

      test(m"A missing certificate alias is Unset"):
        guarded.open[Keystore](Password(t"sesame")):
          keystore.certificate(t"absent")
      . assert(_ == Unset)

      test(m"A keystore opens with a password held by the veiled-heap cloak"):
        import soundness.cloaks.veiledHeapCloak
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
        import soundness.cloaks.heapCloak
        roundtrip()
      . assert(_ == t"Hello world")

      test(m"AES round-trips through the off-heap cloak"):
        import soundness.cloaks.offHeapCloak
        roundtrip()
      . assert(_ == t"Hello world")

      test(m"AES round-trips through the veiled-heap cloak"):
        import soundness.cloaks.veiledHeapCloak
        roundtrip()
      . assert(_ == t"Hello world")

      test(m"AES round-trips through the veiled-off-heap cloak"):
        import soundness.cloaks.veiledOffHeapCloak
        roundtrip()
      . assert(_ == t"Hello world")

      test(m"a password round-trips through the off-heap cloak"):
        import soundness.cloaks.offHeapCloak
        passwordTrip()
      . assert(_ == t"hunter2")

      test(m"a password round-trips through the veiled-heap cloak"):
        import soundness.cloaks.veiledHeapCloak
        passwordTrip()
      . assert(_ == t"hunter2")

      test(m"a password round-trips through the veiled-off-heap cloak"):
        import soundness.cloaks.veiledOffHeapCloak
        passwordTrip()
      . assert(_ == t"hunter2")

      test(m"constructing a password from chars zeroes the input array"):
        val chars = scala.Array('h', 'u', 'n', 't', 'e', 'r', '2')
        val password = Password(chars)
        (chars.forall(_ == '\u0000'), password.uncloak(String(cleartext.chars).tt))
      . assert(_ == (true, t"hunter2"))

      test(m"DSA signing works through an off-heap cloak"):
        import soundness.cloaks.offHeapCloak
        val key = PrivateKey.generate[Dsa[1024]]()
        val signature = key.sign(t"attested")
        key.public.verify(t"attested", signature)
      . assert(_ == true)

      test(m"a symmetric key's material survives multiple uncloaks intact"):
        import soundness.cloaks.veiledOffHeapCloak
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        val first = key.data(Divulgence)
        val second = key.data(Divulgence)
        first.serialize[Hex] == second.serialize[Hex]
      . assert(_ == true)

    suite(m"Asymmetric signatures"):
      test(m"An RSA signature verifies against the matching public key"):
        val key = PrivateKey.generate[Rsa[2048]]()
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"An RSA signature fails against a different key"):
        val key = PrivateKey.generate[Rsa[2048]]()
        val other = PrivateKey.generate[Rsa[2048]]()
        other.public.verify(pangram, key.sign(pangram))
      . assert(!_)

      test(m"An RSA signature fails over different content"):
        val key = PrivateKey.generate[Rsa[2048]]()
        key.public.verify(t"a different message", key.sign(pangram))
      . assert(!_)

      test(m"The signature digest is selectable"):
        import signatureDigests.sha512Signature
        val key = PrivateKey.generate[Rsa[2048]]()
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"A signature made under one digest fails to verify under another"):
        val key = PrivateKey.generate[Rsa[2048]]()
        val signature = key.sign(pangram)
        import signatureDigests.sha512Signature
        key.public.verify(pangram, signature)
      . assert(!_)

      test(m"RSA still encrypts and decrypts"):
        val key = PrivateKey.generate[Rsa[2048]]()
        val message: Data = key.public.uncloak(t"Hello world".encrypt(InitializationVector.random))
        key.uncloak(message.decrypt.as[Text])
      . assert(_ == t"Hello world")

      test(m"An ECDSA signature verifies against the matching public key"):
        val key = PrivateKey.generate[Ecdsa[256]]()
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"ECDSA works over each NIST prime curve"):
        val p256 = PrivateKey.generate[Ecdsa[256]]()
        val p384 = PrivateKey.generate[Ecdsa[384]]()
        val p521 = PrivateKey.generate[Ecdsa[521]]()

        List
         (p256.public.verify(pangram, p256.sign(pangram)),
          p384.public.verify(pangram, p384.sign(pangram)),
          p521.public.verify(pangram, p521.sign(pangram)))
      . assert(_ == List(true, true, true))

      test(m"An ECDSA signature fails against a different key"):
        val key = PrivateKey.generate[Ecdsa[256]]()
        val other = PrivateKey.generate[Ecdsa[256]]()
        other.public.verify(pangram, key.sign(pangram))
      . assert(!_)

      // The JDK does not write RFC 5915's optional public-key field, so enigmatic adds it at
      // generation; this checks the re-encoded key is still a well-formed `PrivateKeyInfo` that
      // the JDK reads back, and that the public key recovered from it is the right one.
      test(m"An ECDSA private key carries its public key in RFC 5915 form"):
        val key = PrivateKey.generate[Ecdsa[256]]()

        val info = key.pem(Divulgence).as[Der].as[Asn1]

        val inner = info match
          case Asn1.Sequence(List(_, _, Asn1.OctetString(octets))) => Der(octets).as[Asn1]
          case _                                                  => Asn1.Null

        inner match
          case Asn1.Sequence(elements) => elements.sweep:
            case tagged: Asn1.Tagged => tagged.tag

          case _ => Nil
      . assert(_ == List(1))

      test(m"An ECDSA public key is a well-formed SubjectPublicKeyInfo"):
        val key = PrivateKey.generate[Ecdsa[256]]()

        Der(key.public.bytes).as[Asn1] match
          case Asn1.Sequence(List(Asn1.Sequence(_), _: Asn1.BitString)) => true
          case _                                                       => false
      . assert(identity)

      test(m"An ML-DSA signature verifies against the matching public key"):
        val key = PrivateKey.generate[MlDsa[65]]()
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"ML-DSA works at each parameter set"):
        val level44 = PrivateKey.generate[MlDsa[44]]()
        val level65 = PrivateKey.generate[MlDsa[65]]()
        val level87 = PrivateKey.generate[MlDsa[87]]()

        List
         (level44.public.verify(pangram, level44.sign(pangram)),
          level65.public.verify(pangram, level65.sign(pangram)),
          level87.public.verify(pangram, level87.sign(pangram)))
      . assert(_ == List(true, true, true))

      test(m"An ML-DSA signature fails against a different key"):
        val key = PrivateKey.generate[MlDsa[65]]()
        val other = PrivateKey.generate[MlDsa[65]]()
        other.public.verify(pangram, key.sign(pangram))
      . assert(!_)

      test(m"An ML-DSA signature fails over different content"):
        val key = PrivateKey.generate[MlDsa[65]]()
        key.public.verify(t"a different message", key.sign(pangram))
      . assert(!_)

      test(m"A tampered ML-DSA signature fails to verify"):
        val key = PrivateKey.generate[MlDsa[65]]()
        val signed = key.sign(pangram).bytes
        val bytes = Array.allocate[Byte](signed.length)
        bytes.place(signed)
        bytes(0) = (bytes.readUnchecked(0) ^ 1).toByte
        key.public.verify(pangram, Signature(Array.freeze(bytes)))
      . assert(!_)

      // The JDK offers no route from an ML-DSA private key back to its public key, so enigmatic
      // embeds the public key at generation, at `OneAsymmetricKey`'s reserved `[1]` position;
      // this checks the re-encoded key still parses and carries exactly that element.
      test(m"An ML-DSA private key carries its public key at RFC 5958's [1] position"):
        val key = PrivateKey.generate[MlDsa[65]]()

        key.pem(Divulgence).as[Der].as[Asn1] match
          case Asn1.Sequence(elements) => elements.sweep:
            case tagged: Asn1.Tagged => tagged.tag

          case _ => Nil
      . assert(_ == List(1))

      test(m"An ML-DSA public key is a well-formed SubjectPublicKeyInfo"):
        val key = PrivateKey.generate[MlDsa[65]]()

        Der(key.public.bytes).as[Asn1] match
          case Asn1.Sequence(List(Asn1.Sequence(_), _: Asn1.BitString)) => true
          case _                                                       => false
      . assert(identity)

    suite(m"Key reading"):
      import KeyFixtures.*

      test(m"An Ed25519 signature verifies against the matching public key"):
        val key = PrivateKey.generate[Ed25519]()
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"An Ed25519 signature fails against a different key"):
        val key = PrivateKey.generate[Ed25519]()
        val other = PrivateKey.generate[Ed25519]()
        other.public.verify(pangram, key.sign(pangram))
      . assert(!_)

      test(m"An OpenSSH Ed25519 key reads and signs"):
        val key = sshEd25519.read[PrivateKey[Ed25519] in Pem]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"An OpenSSH Ed25519 key's public half is its .pub line"):
        val key = sshEd25519.read[PrivateKey[Ed25519] in Pem]
        Array.same(OpenSsh.publicKey[Ed25519](sshEd25519Public).bytes, key.public.bytes)
      . assert(identity)

      test(m"An OpenSSH ECDSA P-256 key reads and signs"):
        val key = sshEcdsa256.read[PrivateKey[Ecdsa[256]] in Pem]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"An OpenSSH ECDSA P-256 key's public half is its .pub line"):
        val key = sshEcdsa256.read[PrivateKey[Ecdsa[256]] in Pem]
        Array.same(OpenSsh.publicKey[Ecdsa[256]](sshEcdsa256Public).bytes, key.public.bytes)
      . assert(identity)

      test(m"An OpenSSH ECDSA P-521 key reads and signs"):
        val key = sshEcdsa521.read[PrivateKey[Ecdsa[521]] in Pem]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"An OpenSSH RSA key reads and signs"):
        val key = sshRsa2048.read[PrivateKey[Rsa[2048]] in Pem]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"An OpenSSH RSA key's public half is its .pub line"):
        val key = sshRsa2048.read[PrivateKey[Rsa[2048]] in Pem]
        Array.same(OpenSsh.publicKey[Rsa[2048]](sshRsa2048Public).bytes, key.public.bytes)
      . assert(identity)

      test(m"An Ed25519 public key renders back to its OpenSSH line"):
        OpenSsh.line(OpenSsh.publicKey[Ed25519](sshEd25519Public), t"test@example")
      . assert(_ == sshEd25519Public)

      test(m"An ECDSA public key renders back to its OpenSSH line"):
        OpenSsh.line(OpenSsh.publicKey[Ecdsa[256]](sshEcdsa256Public), t"test@example")
      . assert(_ == sshEcdsa256Public)

      test(m"An RSA public key renders back to its OpenSSH line"):
        OpenSsh.line(OpenSsh.publicKey[Rsa[2048]](sshRsa2048Public), t"test@example")
      . assert(_ == sshRsa2048Public)

      test(m"A PKCS#8 RSA key reads and signs"):
        val key = pkcs8Rsa2048.read[PrivateKey[Rsa[2048]] in Pem]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"A PKCS#1 RSA key reads and signs"):
        val key = pkcs1Rsa2048.read[PrivateKey[Rsa[2048]] in Pem]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"PKCS#1 and PKCS#8 forms of a key have the same public key"):
        val pkcs1 = pkcs1Rsa2048.read[PrivateKey[Rsa[2048]] in Pem]
        val pkcs8 = pkcs8Rsa2048.read[PrivateKey[Rsa[2048]] in Pem]
        Array.same(pkcs1.public.bytes, pkcs8.public.bytes)
      . assert(identity)

      test(m"A PEM public key is the private key's public half"):
        val key = pkcs8Rsa2048.read[PrivateKey[Rsa[2048]] in Pem]
        Array.same(spkiRsa2048.read[PublicKey[Rsa[2048]] in Pem].bytes, key.public.bytes)
      . assert(identity)

      test(m"A SEC1 EC key reads and signs"):
        val key = sec1P256.read[PrivateKey[Ecdsa[256]] in Pem]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"A SEC1 EC key's signature verifies against its PEM public key"):
        val key = sec1P256.read[PrivateKey[Ecdsa[256]] in Pem]
        spkiP256.read[PublicKey[Ecdsa[256]] in Pem].verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"A PKCS#8 EC P-384 key reads and signs"):
        val key = pkcs8P384.read[PrivateKey[Ecdsa[384]] in Pem]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"A key read as a PEM block, then decoded, is the same key"):
        val key = pkcs8Rsa2048.read[Pem].as[PrivateKey[Rsa[2048]]]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"A key read from DER is the same key"):
        val key = pkcs8Rsa2048.read[Pem].as[Der].as[PrivateKey[Rsa[2048]]]
        key.public.verify(pangram, key.sign(pangram))
      . assert(identity)

      test(m"Reading a key as the wrong size is a mismatch"):
        capture[PrivateKey.Error](pkcs8Rsa2048.read[PrivateKey[Rsa[4096]] in Pem]).reason
      . assert(_ == PrivateKey.Error.Reason.Mismatch(t"RSA-4096", t"RSA-2048"))

      test(m"Reading a key as the wrong algorithm is a mismatch"):
        capture[PrivateKey.Error](sshEd25519.read[PrivateKey[Rsa[2048]] in Pem]).reason
      . assert(_ == PrivateKey.Error.Reason.Mismatch(t"RSA-2048", t"Ed25519"))

      test(m"Reading an EC key over the wrong curve is a mismatch"):
        capture[PrivateKey.Error](sec1P256.read[PrivateKey[Ecdsa[384]] in Pem]).reason
      . assert(_ == PrivateKey.Error.Reason.Mismatch(t"ECDSA P-384", t"ECDSA P-256"))

      test(m"Reading a public key as the wrong algorithm is a mismatch"):
        capture[PublicKey.Error](spkiEd25519.read[PublicKey[Ecdsa[256]] in Pem]).reason
      . assert(_ == PublicKey.Error.Reason.Mismatch(t"ECDSA P-256", t"Ed25519"))

      test(m"Reading an OpenSSH line as the wrong algorithm is a mismatch"):
        capture[PublicKey.Error](OpenSsh.publicKey[Rsa[2048]](sshEd25519Public)).reason
      . assert(_ == PublicKey.Error.Reason.Mismatch(t"RSA-2048", t"Ed25519"))

      test(m"A passphrase-protected OpenSSH key is reported as encrypted"):
        capture[PrivateKey.Error](sshEd25519Encrypted.read[PrivateKey[Ed25519] in Pem]).reason
      . assert(_ == PrivateKey.Error.Reason.Encrypted)

      test(m"A PBES2-encrypted PKCS#8 key is reported as encrypted"):
        capture[PrivateKey.Error](pkcs8Rsa2048Encrypted.read[PrivateKey[Rsa[2048]] in Pem]).reason
      . assert(_ == PrivateKey.Error.Reason.Encrypted)

      test(m"An Ed25519 PKCS#8 key without its public key is rejected"):
        capture[PrivateKey.Error](pkcs8Ed25519.read[PrivateKey[Ed25519] in Pem]).reason
      . assert(_ == PrivateKey.Error.Reason.PublicKeyAbsent)

      test(m"A public-key block read as a private key is a mismatch"):
        capture[PrivateKey.Error](spkiRsa2048.read[PrivateKey[Rsa[2048]] in Pem]).reason
      . assert(_ == PrivateKey.Error.Reason.Mismatch(t"private", t"a public key block"))

      test(m"A generated Ed25519 key round-trips through PEM"):
        val key = PrivateKey.generate[Ed25519]()
        val read = key.pem(Divulgence).serialize.read[PrivateKey[Ed25519] in Pem]
        Array.same(read.public.bytes, key.public.bytes)
      . assert(identity)

      test(m"A generated ECDSA key round-trips through PEM"):
        val key = PrivateKey.generate[Ecdsa[256]]()
        val read = key.pem(Divulgence).serialize.read[PrivateKey[Ecdsa[256]] in Pem]
        Array.same(read.public.bytes, key.public.bytes)
      . assert(identity)

    suite(m"Key agreement and authenticated encryption"):
      // RFC 7748 §6.1: Alice's and Bob's keys, and the secret they agree on.
      val alicePrivate = t"77076D0A7318A57D3C16C17251B26645DF4C2F87EBC0992AB177FBA51DB92C2A"
      val alicePublic = t"8520F0098930A754748B7DDCB43EF75A0DBF3A0D26381AF4EBA4A98EAA9B4E6A"
      val bobPrivate = t"5DAB087E624A8A4B79E17F8B83800EE66F3BB1292618B6FD1C2F8B27FF88E0EB"
      val bobPublic = t"DE9EDB7D7B7DC1B4D35B61C2ECE435373F8343C85B78674DADFC7E146F882B4F"
      val shared = t"4A5D9D5BA4CE2DE1728E3BF480350F25E07E21C947D19E3376F09B3C1E161742"

      // An X25519 private key in PKCS#8 form carrying its public key, as `PrivateKey.generate`
      // writes one, from RFC 7748's raw scalar and point.
      def x25519Key(scalar: Text, point: Text): PrivateKey[X25519] =
        val seed: Asn1 = Asn1.OctetString(scalar.deserialize[Hex])
        val public = Asn1.Tagged(1, true, Asn1.BitString(point.deserialize[Hex], 0))
        val identifier = Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.X25519)))
        val octets = Asn1.OctetString(seed.in[Der].data)
        val info: Asn1 = Asn1.Sequence(List(Asn1.Integer(0), identifier, octets, public))
        info.in[Der].as[PrivateKey[X25519]]

      test(m"Two X25519 keys agree on the same secret"):
        val alice = PrivateKey.generate[X25519]()
        val bob = PrivateKey.generate[X25519]()
        alice.agree(bob.public): one =>
          bob.agree(alice.public): other =>
            Array.same(one, other) && one.length == 32
      . assert(identity)

      test(m"A third X25519 key agrees on a different secret"):
        val alice = PrivateKey.generate[X25519]()
        val bob = PrivateKey.generate[X25519]()
        val eve = PrivateKey.generate[X25519]()
        alice.agree(bob.public)(one => eve.agree(alice.public)(other => Array.same(one, other)))
      . assert(!_)

      test(m"X25519 agrees on RFC 7748's shared secret"):
        val alice = x25519Key(alicePrivate, alicePublic)
        PublicKey.raw[X25519](bobPublic.deserialize[Hex]).let: bob =>
          alice.agree(bob)(_.serialize[Hex])
      . assert(_ == shared)

      test(m"X25519 agrees on RFC 7748's shared secret from Bob's side"):
        val bob = x25519Key(bobPrivate, bobPublic)
        PublicKey.raw[X25519](alicePublic.deserialize[Hex]).let: alice =>
          bob.agree(alice)(_.serialize[Hex])
      . assert(_ == shared)

      test(m"An X25519 public key's raw form is its 32-byte point"):
        x25519Key(alicePrivate, alicePublic).public.raw.let(_.serialize[Hex])
      . assert(_ == alicePublic)

      test(m"A raw public key round-trips through its document form"):
        val key = PrivateKey.generate[X25519]()
        val raw = key.public.raw.or(Data())
        PublicKey.raw[X25519](raw).let(_.bytes).let(Array.same(_, key.public.bytes))
      . assert(_ == true)

      test(m"Two ECDH P-256 keys agree on the same 32-byte secret"):
        val alice = PrivateKey.generate[Ecdh[256]]()
        val bob = PrivateKey.generate[Ecdh[256]]()
        alice.agree(bob.public): one =>
          bob.agree(alice.public): other =>
            Array.same(one, other) && one.length == 32
      . assert(identity)

      test(m"Two ECDH P-384 keys agree on the same 48-byte secret"):
        val alice = PrivateKey.generate[Ecdh[384]]()
        val bob = PrivateKey.generate[Ecdh[384]]()
        alice.agree(bob.public): one =>
          bob.agree(alice.public): other =>
            Array.same(one, other) && one.length == 48
      . assert(identity)

      test(m"An ECDH public key's raw form is an uncompressed point"):
        val key = PrivateKey.generate[Ecdh[256]]()
        key.public.raw.let(raw => raw.length == 65 && raw.readUnchecked(0) == 4)
      . assert(_ == true)

      test(m"An ECDSA key file reads as an ECDH key too"):
        import KeyFixtures.sec1P256
        val key = sec1P256.read[PrivateKey[Ecdh[256]] in Pem]
        val other = PrivateKey.generate[Ecdh[256]]()
        key.agree(other.public)(one => other.agree(key.public)(other => Array.same(one, other)))
      . assert(identity)

      val nonce: Data = Array.fill[Byte](12)(0)

      test(m"AES-GCM seals and opens with associated data"):
        val key = SymmetricKey.generate[AesGcm[256]]()
        val box = key.seal(nonce, t"header".bytestream, pangram.bytestream)
        key.unseal(nonce, t"header".bytestream, box).read[Text]
      . assert(_ == pangram)

      test(m"AES-GCM's ciphertext is the plaintext plus a 16-byte tag"):
        val key = SymmetricKey.generate[AesGcm[128]]()
        key.seal(nonce, Data(), pangram.bytestream).length - pangram.bytestream.length
      . assert(_ == 16)

      test(m"AES-GCM rejects a tampered ciphertext"):
        val key = SymmetricKey.generate[AesGcm[256]]()
        val box = key.seal(nonce, Data(), pangram.bytestream)
        val tampered = Array.unsafeJvm(box).clone
        tampered(0) = (tampered(0) ^ 1).toByte
        capture[Crypto.Error](key.unseal(nonce, Data(), Array.unsafeFrozen(tampered))).reason
      . assert(_ == Crypto.Error.Reason.BadTag)

      test(m"AES-GCM rejects different associated data"):
        val key = SymmetricKey.generate[AesGcm[256]]()
        val box = key.seal(nonce, t"header".bytestream, pangram.bytestream)
        capture[Crypto.Error](key.unseal(nonce, t"other".bytestream, box)).reason
      . assert(_ == Crypto.Error.Reason.BadTag)

      test(m"AES-GCM rejects a different nonce"):
        val key = SymmetricKey.generate[AesGcm[256]]()
        val box = key.seal(nonce, Data(), pangram.bytestream)
        val other: Data = Array.fill[Byte](12)(1)
        capture[Crypto.Error](key.unseal(other, Data(), box)).reason
      . assert(_ == Crypto.Error.Reason.BadTag)

      // NIST GCM specification test case 2: an all-zero key, nonce and block.
      test(m"AES-128-GCM matches NIST's test case 2"):
        val key = SymmetricKey[AesGcm[128]](Array.fill[Byte](16)(0))
        key.seal(nonce, Data(), Array.fill[Byte](16)(0)).serialize[Hex]
      . assert(_ == t"0388DACE60B6A392F328C2B971B2FE78AB6E47D42CEC13BDF53A67B21257BDDF")

    suite(m"ChaCha20 and Poly1305"):
      // RFC 8439's vectors: §2.4.2 (the keystream), §2.5.2 (the authenticator) and §2.8.2 (the AEAD).
      val sunscreen = t"Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."

      test(m"ChaCha20 produces RFC 8439's keystream"):
        val key = SymmetricKey[ChaCha20](t"000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F".deserialize[Hex])
        key.xor(t"000000000000004A00000000".deserialize[Hex], 1, sunscreen.bytestream).serialize[Hex]
      . assert(_ == t"6E2E359A2568F98041BA0728DD0D6981E97E7AEC1D4360C20A27AFCCFD9FAE0BF91B65C5524733AB8F593DABCD62B3571639D624E65152AB8F530C359F0861D807CA0DBF500D6A6156A38E088A22B65E52BC514D16CCF806818CE91AB77937365AF90BBF74A35BE6B40B8EEDF2785E42874D")

      test(m"Applying the keystream twice restores the plaintext"):
        val key = SymmetricKey.generate[ChaCha20]()
        val nonce: Data = Array.fill[Byte](12)(7)
        key.xor(nonce, 1, key.xor(nonce, 1, sunscreen.bytestream)).read[Text]
      . assert(_ == sunscreen)

      test(m"Poly1305 produces RFC 8439's tag"):
        val key = t"85D6BE7857556D337F4452FE42D506A80103808AFB0DB2FD4ABFF6AF4149F51B".deserialize[Hex]
        Poly1305.mac(key, t"Cryptographic Forum Research Group".bytestream).serialize[Hex]
      . assert(_ == t"A8061DC1305136C6C22B8BAF0C0127A9")

      test(m"Poly1305 of an empty message is the key's second half"):
        val key = t"85D6BE7857556D337F4452FE42D506A80103808AFB0DB2FD4ABFF6AF4149F51B".deserialize[Hex]
        Poly1305.mac(key, Data()).serialize[Hex]
      . assert(_ == t"0103808AFB0DB2FD4ABFF6AF4149F51B")

      test(m"Poly1305 agrees with the JDK's fused ChaCha20-Poly1305 on every length"):
        // RFC 8439 §2.8: the tag is Poly1305, under block zero of the keystream, over the padded
        // associated data, the padded ciphertext, and their lengths.
        val key = SymmetricKey.generate[ChaCha20]()
        val aead = SymmetricKey[ChaCha20Poly1305](key.data(Divulgence))
        val nonce: Data = Array.fill[Byte](12)(3)

        def join(parts: Data*): Data =
          Array.unsafeFrozen(parts.map(Array.unsafeJvm(_)).reduce(_ ++ _))

        def pad(data: Data): Data =
          if data.length % 16 == 0 then data else join(data, Array.fill[Byte](16 - data.length % 16)(0))

        def le64(value: Long): Data = Array.tabulate[Byte](8)(index => (value >>> (8*index)).toByte)

        (0 until 40).forall: length =>
          val plaintext: Data = Array.tabulate[Byte](length)(_.toByte)
          val associated: Data = Array.tabulate[Byte](length % 7)(index => (index + 100).toByte)
          val box = aead.seal(nonce, associated, plaintext)
          val ciphertext = box.keep(length)
          val tag = box.skip(length)
          val otk = key.xor(nonce, 0, Array.fill[Byte](32)(0))
          val authenticated =
            join(pad(associated), pad(ciphertext), le64(associated.length), le64(length))
          Array.same(Poly1305.mac(otk, authenticated), tag)
      . assert(identity)

      test(m"ChaCha20-Poly1305 produces RFC 8439's ciphertext and tag"):
        val key = SymmetricKey[ChaCha20Poly1305](t"808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F".deserialize[Hex])
        val nonce = t"070000004041424344454647".deserialize[Hex]
        val associated = t"50515253C0C1C2C3C4C5C6C7".deserialize[Hex]
        key.seal(nonce, associated, sunscreen.bytestream).serialize[Hex]
      . assert(_ == t"D31A8D34648E60DB7B86AFBC53EF7EC2A4ADED51296E08FEA9E2B5A736EE62D63DBEA45E8CA9671282FAFB69DA92728B1A71DE0A9E060B2905D6A5B67ECD3B3692DDBD7F2D778B8C9803AEE328091B58FAB324E4FAD675945585808B4831D7BC3FF4DEF08E4B7A9DE576D26586CEC64B61161AE10B594F09E26A7E902ECBD0600691")

      test(m"ChaCha20-Poly1305 rejects a tampered tag"):
        val key = SymmetricKey.generate[ChaCha20Poly1305]()
        val nonce: Data = Array.fill[Byte](12)(0)
        val box = key.seal(nonce, Data(), sunscreen.bytestream)
        val tampered = Array.unsafeJvm(box).clone
        tampered(tampered.length - 1) = (tampered(tampered.length - 1) ^ 1).toByte
        capture[Crypto.Error](key.unseal(nonce, Data(), Array.unsafeFrozen(tampered))).reason
      . assert(_ == Crypto.Error.Reason.BadTag)

    suite(m"Pure-Scala provider (SoundnessCrypto)"):
      // The pure-Scala provider is checked two ways: against the standards' published vectors, and
      // against the JDK provider, in both directions, so that a ciphertext, tag, key or signature
      // made by one is accepted by the other.
      val pure = SoundnessCrypto
      val jdk = JavaBaseCrypto
      val sunscreen = t"Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."

      def bytes(length: Int, seed: Int): Data = Array.tabulate[Byte](length)(index => (index*7 + seed).toByte)

      test(m"AES-128 encrypts FIPS 197's appendix C.1 block"):
        val key = t"000102030405060708090A0B0C0D0E0F".deserialize[Hex]
        val plaintext = t"00112233445566778899AABBCCDDEEFF".deserialize[Hex]
        pure.aes.encrypt(t"AES/ECB/NoPadding", key, Unset, plaintext).serialize[Hex]
      . assert(_ == t"69C4E0D86A7B0430D8CDB78070B4C55A")

      test(m"AES-256 encrypts FIPS 197's appendix C.3 block"):
        val key = t"000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F".deserialize[Hex]
        val plaintext = t"00112233445566778899AABBCCDDEEFF".deserialize[Hex]
        pure.aes.encrypt(t"AES/ECB/NoPadding", key, Unset, plaintext).serialize[Hex]
      . assert(_ == t"8EA2B7CA516745BFEAFC49904B496089")

      test(m"AES-256 decrypts FIPS 197's appendix C.3 block"):
        val key = t"000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F".deserialize[Hex]
        pure.aes.decrypt(t"AES/ECB/NoPadding", key, Unset, t"8EA2B7CA516745BFEAFC49904B496089".deserialize[Hex]).serialize[Hex]
      . assert(_ == t"00112233445566778899AABBCCDDEEFF")

      val modes = List(t"CBC", t"ECB", t"CTR", t"CFB", t"OFB")
      val paddings = List(t"PKCS5Padding", t"ISO10126Padding", t"NoPadding")
      val keySizes = List(16, 24, 32)
      val lengths = List(0, 1, 15, 16, 17, 31, 32, 33, 100)

      // `NoPadding` needs whole blocks in the block-structured modes, and the JDK refuses to pad
      // CTR at all (which the pure provider mirrors).
      def aligned(mode: Text, padding: Text, length: Int): Boolean =
        (padding != t"NoPadding" || !(mode == t"CBC" || mode == t"ECB") || length%16 == 0)
        && (mode != t"CTR" || padding == t"NoPadding")

      def ivOf(mode: Text): Optional[Data] = if mode == t"ECB" then Unset else bytes(16, 3)
      def ivSize(mode: Text): Optional[Int] = if mode == t"ECB" then Unset else 16

      test(m"Every AES mode, padding and key size the JDK decrypts matches the pure provider's"):
        modes.all: mode =>
          paddings.all: padding =>
            keySizes.all: keySize =>
              lengths.filter(aligned(mode, padding, _)).all: length =>
                val transformation = t"AES/$mode/$padding"
                val key = bytes(keySize, 11)
                val plaintext = bytes(length, 5)
                val ciphertext = pure.aes.encrypt(transformation, key, ivOf(mode), plaintext)
                val decrypted = jdk.aes.decrypt(transformation, key, ivSize(mode), ciphertext)
                val reverse = pure.aes.decrypt(transformation, key, ivSize(mode), jdk.aes.encrypt(transformation, key, ivOf(mode), plaintext))
                Array.same(decrypted, plaintext) && Array.same(reverse, plaintext)
      . assert(identity)

      test(m"Deterministic AES paddings produce the JDK's exact ciphertext"):
        modes.all: mode =>
          List(t"PKCS5Padding", t"NoPadding").all: padding =>
            lengths.filter(aligned(mode, padding, _)).all: length =>
              val transformation = t"AES/$mode/$padding"
              val key = bytes(32, 13)
              val plaintext = bytes(length, 9)
              val ours = pure.aes.encrypt(transformation, key, ivOf(mode), plaintext)
              val theirs = jdk.aes.encrypt(transformation, key, ivOf(mode), plaintext)
              Array.same(ours, theirs)
      . assert(identity)

      test(m"AES streaming sessions match the whole-value form for every chunking"):
        modes.all: mode =>
          paddings.filter(_ != t"ISO10126Padding").filter(aligned(mode, _, 96)).all: padding =>
            List(1, 5, 16, 17, 64).all: chunk =>
              val transformation = t"AES/$mode/$padding"
              val key = bytes(16, 17)
              val plaintext = bytes(96, 21)
              val whole = pure.aes.encrypt(transformation, key, ivOf(mode), plaintext)
              val session = pure.aes.stream(transformation, key, ivOf(mode))
              val parts = (0 until 96 by chunk).map { start => session.update(plaintext.skip(start).keep(chunk)) }
              val prefix = ivOf(mode).lay(scala.collection.immutable.Seq[Data]())(scala.collection.immutable.Seq(_))
              val streamed = SoundnessBlockCipher.join((prefix ++ parts :+ session.finish())*)
              val decrypting = pure.aes.decryptStream(transformation, key, ivOf(mode))
              val body = whole.skip(ivSize(mode).or(0))
              val decryptedParts = (0 until body.length by chunk).map { start => decrypting.update(body.skip(start).keep(chunk)) }
              val unstreamed = SoundnessBlockCipher.join((decryptedParts :+ decrypting.finish())*)
              Array.same(streamed, whole) && Array.same(unstreamed, plaintext)
      . assert(identity)

      test(m"The pure provider rejects corrupted PKCS#7 padding as BadPadding"):
        val key = bytes(16, 1)
        val ciphertext = pure.aes.encrypt(t"AES/CBC/PKCS5Padding", key, bytes(16, 2), bytes(20, 3))
        val tampered = Array.unsafeJvm(ciphertext).clone
        tampered(tampered.length - 1) = (tampered(tampered.length - 1) ^ 0x55).toByte
        try
          pure.aes.decrypt(t"AES/CBC/PKCS5Padding", key, 16, Array.unsafeFrozen(tampered))
          Unset
        catch case error: Exception => failureReason(error)
      . assert(_ == Crypto.Error.Reason.BadPadding)

      test(m"HMAC-SHA-256 produces RFC 4231's first test case"):
        pure.hmac(t"HmacSHA256").mac(Array.fill[Byte](20)(0x0b), t"Hi There".bytestream).serialize[Hex]
      . assert(_ == t"B0344C61D8DB38535CA8AFCEAF0BF12B881DC200C9833DA726E9376C2E32CFF7")

      test(m"HMAC-SHA-512 produces RFC 4231's seventh test case (a long key)"):
        val key = Array.fill[Byte](131)(0xaa.toByte)
        val message = t"This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.".bytestream
        pure.hmac(t"HmacSHA512").mac(key, message).serialize[Hex]
      . assert(_ == t"E37B6A775DC87DBAA4DFA9F96E5E3FFDDEBD71F8867289865DF5A32D20CDC944B6022CAC3C4982B10D5EEB55C3E4DE15134676FB6DE0446065C97440FA8C6A58")

      test(m"Every HMAC matches the JDK's for short, block-sized and long keys"):
        List(t"HmacMD5", t"HmacSHA1", t"HmacSHA224", t"HmacSHA256", t"HmacSHA384", t"HmacSHA512").all: name =>
          List(1, 20, 63, 64, 65, 127, 128, 129, 300).all: keyLength =>
            val key = bytes(keyLength, 2)
            Array.same(pure.hmac(name).mac(key, sunscreen.bytestream), jdk.hmac(name).mac(key, sunscreen.bytestream))
      . assert(identity)

      test(m"AES-GCM produces NIST's test case 2"):
        val zeros16 = Array.fill[Byte](16)(0)
        pure.aesGcm.seal(zeros16, Array.fill[Byte](12)(0), Data(), zeros16).serialize[Hex]
      . assert(_ == t"0388DACE60B6A392F328C2B971B2FE78AB6E47D42CEC13BDF53A67B21257BDDF")

      test(m"AES-GCM produces NIST's test case 4 (associated data, partial block)"):
        val key = t"FEFFE9928665731C6D6A8F9467308308".deserialize[Hex]
        val nonce = t"CAFEBABEFACEDBADDECAF888".deserialize[Hex]
        val plaintext = t"D9313225F88406E5A55909C5AFF5269A86A7A9531534F7DA2E4C303D8A318A721C3C0C95956809532FCF0E2449A6B525B16AEDF5AA0DE657BA637B39".deserialize[Hex]
        val associated = t"FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2".deserialize[Hex]
        pure.aesGcm.seal(key, nonce, associated, plaintext).serialize[Hex]
      . assert(_ == t"42831EC2217774244B7221B784D0D49CE3AA212F2C02A4E035C17E2329ACA12E21D514B25466931C7D8F6A5AAC84AA051BA30B396A0AAC973D58E091" + t"5BC94FBC3221A5DB94FAE95AE7121A47")

      test(m"AES-GCM interoperates with the JDK in both directions, and rejects tampering"):
        List(16, 32).all: keySize =>
          List(0, 1, 15, 16, 17, 40).all: length =>
            val key = bytes(keySize, 4)
            val nonce = bytes(12, 6)
            val associated = bytes(length%7, 8)
            val plaintext = bytes(length, 10)
            val ours = pure.aesGcm.seal(key, nonce, associated, plaintext)
            val theirs = jdk.aesGcm.seal(key, nonce, associated, plaintext)
            val tampered = Array.unsafeJvm(ours).clone
            tampered(0) = (tampered(0) ^ 1).toByte
            Array.same(ours, theirs)
            && jdk.aesGcm.unseal(key, nonce, associated, ours).lay(false)(Array.same(_, plaintext))
            && pure.aesGcm.unseal(key, nonce, associated, theirs).lay(false)(Array.same(_, plaintext))
            && pure.aesGcm.unseal(key, nonce, associated, Array.unsafeFrozen(tampered)).absent
      . assert(identity)

      test(m"The pure ChaCha20 produces RFC 8439's keystream"):
        val key = t"000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F".deserialize[Hex]
        pure.chaCha20.xor(key, t"000000000000004A00000000".deserialize[Hex], 1, sunscreen.bytestream).serialize[Hex]
      . assert(_ == t"6E2E359A2568F98041BA0728DD0D6981E97E7AEC1D4360C20A27AFCCFD9FAE0BF91B65C5524733AB8F593DABCD62B3571639D624E65152AB8F530C359F0861D807CA0DBF500D6A6156A38E088A22B65E52BC514D16CCF806818CE91AB77937365AF90BBF74A35BE6B40B8EEDF2785E42874D")

      test(m"The pure ChaCha20-Poly1305 produces RFC 8439's ciphertext and tag"):
        val key = t"808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F".deserialize[Hex]
        val nonce = t"070000004041424344454647".deserialize[Hex]
        val associated = t"50515253C0C1C2C3C4C5C6C7".deserialize[Hex]
        pure.chaCha20Poly1305.seal(key, nonce, associated, sunscreen.bytestream).serialize[Hex]
      . assert(_ == t"D31A8D34648E60DB7B86AFBC53EF7EC2A4ADED51296E08FEA9E2B5A736EE62D63DBEA45E8CA9671282FAFB69DA92728B1A71DE0A9E060B2905D6A5B67ECD3B3692DDBD7F2D778B8C9803AEE328091B58FAB324E4FAD675945585808B4831D7BC3FF4DEF08E4B7A9DE576D26586CEC64B61161AE10B594F09E26A7E902ECBD0600691")

      test(m"ChaCha20-Poly1305 interoperates with the JDK in both directions"):
        List(0, 1, 15, 16, 17, 64, 65).all: length =>
          val key = bytes(32, 12)
          val nonce = bytes(12, 14)
          val associated = bytes(length%5, 16)
          val plaintext = bytes(length, 18)
          val ours = pure.chaCha20Poly1305.seal(key, nonce, associated, plaintext)
          val theirs = jdk.chaCha20Poly1305.seal(key, nonce, associated, plaintext)
          Array.same(ours, theirs)
          && jdk.chaCha20Poly1305.unseal(key, nonce, associated, ours).lay(false)(Array.same(_, plaintext))
          && pure.chaCha20Poly1305.unseal(key, nonce, associated, theirs).lay(false)(Array.same(_, plaintext))
      . assert(identity)

      // RFC 7748 §6.1's keys, in the PKCS#8 and SubjectPublicKeyInfo forms the provider contract uses.
      def pkcs8(oid: List[Int], secret: Text, public: Text): Data =
        val seed: Asn1 = Asn1.OctetString(secret.deserialize[Hex])
        val tagged = Asn1.Tagged(1, true, Asn1.BitString(public.deserialize[Hex], 0))
        val identifier = Asn1.Sequence(List(Asn1.ObjectId(oid)))
        val info: Asn1 = Asn1.Sequence(List(Asn1.Integer(0), identifier, Asn1.OctetString(seed.in[Der].data), tagged))
        info.in[Der].data

      def spki(oid: List[Int], public: Text): Data =
        val info: Asn1 = Asn1.Sequence(List(Asn1.Sequence(List(Asn1.ObjectId(oid))), Asn1.BitString(public.deserialize[Hex], 0)))
        info.in[Der].data

      test(m"The pure X25519 agrees on RFC 7748's shared secret"):
        val alice = pkcs8(KeyAlgorithm.X25519, t"77076D0A7318A57D3C16C17251B26645DF4C2F87EBC0992AB177FBA51DB92C2A", t"8520F0098930A754748B7DDCB43EF75A0DBF3A0D26381AF4EBA4A98EAA9B4E6A")
        val bob = spki(KeyAlgorithm.X25519, t"DE9EDB7D7B7DC1B4D35B61C2ECE435373F8343C85B78674DADFC7E146F882B4F")
        pure.x25519.agree(alice, bob).serialize[Hex]
      . assert(_ == t"4A5D9D5BA4CE2DE1728E3BF480350F25E07E21C947D19E3376F09B3C1E161742")

      test(m"The pure X25519 derives RFC 7748's public key from the private scalar"):
        val alice = pkcs8(KeyAlgorithm.X25519, t"77076D0A7318A57D3C16C17251B26645DF4C2F87EBC0992AB177FBA51DB92C2A", t"8520F0098930A754748B7DDCB43EF75A0DBF3A0D26381AF4EBA4A98EAA9B4E6A")
        Array.same(pure.x25519.privateToPublic(alice), spki(KeyAlgorithm.X25519, t"8520F0098930A754748B7DDCB43EF75A0DBF3A0D26381AF4EBA4A98EAA9B4E6A"))
      . assert(identity)

      test(m"Pure and JDK X25519 keys agree with each other"):
        val ours = pure.x25519.generateKeyPair(256)
        val theirs = jdk.x25519.generateKeyPair(256)
        val one = pure.x25519.agree(ours, jdk.x25519.privateToPublic(theirs))
        val other = jdk.x25519.agree(theirs, pure.x25519.privateToPublic(ours))
        Array.same(one, other) && one.length == 32 && Array.same(jdk.x25519.privateToPublic(ours), pure.x25519.privateToPublic(ours))
      . assert(identity)

      test(m"The pure Ed25519 produces RFC 8032's first test signature"):
        val key = pkcs8(KeyAlgorithm.Ed25519, t"9D61B19DEFFD5A60BA844AF492EC2CC44449C5697B326919703BAC031CAE7F60", t"D75A980182B10AB7D54BFED3C964073A0EE172F3DAA62325AF021A68F707511A")
        pure.ed25519.sign(Data(), key).serialize[Hex]
      . assert(_ == t"E5564300C360AC729086E2CC806E828A84877F1EB8E5D974D873E065224901555FB8821590A33BACC61E39701CF9B46BD25BF5F0595BBE24655141438E7A100B")

      test(m"The pure Ed25519 produces RFC 8032's second test signature and derives its public key"):
        val key = pkcs8(KeyAlgorithm.Ed25519, t"4CCD089B28FF96DA9DB6C346EC114E0F5B8A319F35ABA624DA8CF6ED4FB8A6FB", t"3D4017C3E843895A92B70AA74D1B7EBC9C982CCF2EC4968CC0CD55F12AF4660C")
        val public = spki(KeyAlgorithm.Ed25519, t"3D4017C3E843895A92B70AA74D1B7EBC9C982CCF2EC4968CC0CD55F12AF4660C")
        val message = t"72".deserialize[Hex]
        val signature = pure.ed25519.sign(message, key)
        signature.serialize[Hex] == t"92A009A9F0D4CAB8720E820B5F642540A2B27B5416503F8FB3762223EBDB69DA085AC1E43E15996E458F3613D0F11D8C387B2EAEB4302AEEB00D291612BB0C00"
        && Array.same(pure.ed25519.privateToPublic(key), public)
        && pure.ed25519.verify(message, signature, public)
        && !pure.ed25519.verify(t"73".deserialize[Hex], signature, public)
      . assert(identity)

      test(m"Pure and JDK Ed25519 signatures verify under each other"):
        val ours = pure.ed25519.generateKeyPair(256)
        val theirs = jdk.ed25519.generateKeyPair(256)
        val message = sunscreen.bytestream
        jdk.ed25519.verify(message, pure.ed25519.sign(message, ours), pure.ed25519.privateToPublic(ours))
        && pure.ed25519.verify(message, jdk.ed25519.sign(message, theirs), jdk.ed25519.privateToPublic(theirs))
        && pure.ed25519.verify(message, jdk.ed25519.sign(message, ours), jdk.ed25519.privateToPublic(ours))
        && Array.same(jdk.ed25519.privateToPublic(ours), pure.ed25519.privateToPublic(ours))
      . assert(identity)

      test(m"The typed API runs on the pure provider through `providers.soundnessProvider`"):
        given SoundnessCrypto.type = SoundnessCrypto
        val key = SymmetricKey.generate[AesGcm[256]]()
        val nonce: Data = Array.fill[Byte](12)(1)
        key.unseal(nonce, Data(), key.seal(nonce, Data(), sunscreen.bytestream)).read[Text]
      . assert(_ == sunscreen)

    suite(m"X.509 certificates"):
      import java.io as ji
      import java.security as js, js.cert as jsc

      import chronometries.unixChronometry

      val subject = Distinguished
                     ( commonName = t"asn1.example.com",
                       organization = t"Propensive",
                       country = t"GB" )

      val period = Instant(1_600_000_000_000L) ~ Instant(1_900_000_000_000L)

      def certificate(): Certificate =
        val key = PrivateKey.generate[Rsa[2048]]()
        Certificate.selfSigned(subject, key, period, BigInt(1), alternatives = List(t"example.com"))

      // The JDK's X.509 implementation is an independent reader of what enigmatic writes: it
      // parses the structure, decodes every field, and — through `verify` — checks the signature
      // over the exact `TBSCertificate` bytes it re-derived from the encoding. `openssl x509` and
      // `openssl verify` agree with it, but cannot be run from here.
      def parse(certificate: Certificate): jsc.X509Certificate =
        val factory = jsc.CertificateFactory.getInstance("X.509").nn
        val bytes = ji.ByteArrayInputStream(Array.unsafeJvm(certificate.in[Der].data))

        factory.generateCertificate(bytes).nn.asInstanceOf[jsc.X509Certificate]

      def publicKey(certificate: Certificate): js.PublicKey = parse(certificate).getPublicKey.nn

      test(m"The JDK parses a generated certificate and checks its signature"):
        val certificate0 = certificate()
        parse(certificate0).verify(publicKey(certificate0))
        true
      test(m"The JDK parses an Ed25519 self-signed certificate and checks its signature"):
        val key = PrivateKey.generate[Ed25519]()
        val certificate0 = Certificate.selfSigned(subject, key, period, BigInt(7))
        parse(certificate0).verify(publicKey(certificate0))
        true
      . assert(identity)

      test(m"The JDK checks an ECDSA certificate's signature"):
        val key = PrivateKey.generate[Ecdsa[256]]()
        val certificate0 = Certificate.selfSigned(subject, key, period, BigInt(3))
        parse(certificate0).verify(publicKey(certificate0))
        true
      . assert(identity)

      test(m"A tampered certificate fails the JDK's signature check"):
        val certificate0 = certificate()
        val data = certificate0.in[Der].data
        val last = data.length - 1
        val bytes = Array.allocate[Byte](data.length)
        bytes.place(data)
        // Flip a bit inside the signature, which the structure keeps well-formed.
        bytes(last) = (bytes.readUnchecked(last) ^ 0xff.toByte).toByte
        val tampered = Certificate(Der(Array.freeze(bytes)).as[Asn1])

        try
          parse(tampered).verify(publicKey(certificate0))
          false
        catch case _: js.GeneralSecurityException => true
      . assert(identity)

      test(m"The JDK reads back the subject, serial number and validity"):
        val parsed = parse(certificate())

        List
         ( parsed.getSubjectX500Principal.nn.getName.nn.tt,
           parsed.getSerialNumber.nn.toString.nn.tt,
           parsed.getNotBefore.nn.getTime.toString.tt )
      . assert(_ == List(t"CN=asn1.example.com,O=Propensive,C=GB", t"1", t"1600000000000"))

      test(m"The JDK reads back the subject alternative names"):
        val names = parse(certificate()).getSubjectAlternativeNames.nn
        names.size
      . assert(_ == 1)

      test(m"A certificate authority is marked as one"):
        val key = PrivateKey.generate[Rsa[2048]]()

        val authority =
          Certificate.selfSigned(subject, key, period, BigInt(9), authority = true)

        // `getBasicConstraints` returns the path-length constraint for a CA, and -1 otherwise.
        (parse(authority).getBasicConstraints, parse(certificate()).getBasicConstraints)
      . assert(_ == (Int.MaxValue, -1))

      test(m"A self-signed certificate is a well-formed X.509 structure"):
        certificate().asn1 match
          case Asn1.Sequence(List(Asn1.Sequence(_), Asn1.Sequence(_), _: Asn1.BitString)) => true
          case _                                                                          => false
      . assert(identity)

      test(m"A certificate round-trips through DER byte-exactly"):
        val der = certificate().in[Der]
        der.as[Certificate].in[Der] == der
      . assert(identity)

      test(m"A certificate round-trips through PEM"):
        val original = certificate()
        val read: Certificate = original.pem.serialize.read[Certificate in Pem]
        read.in[Der] == original.in[Der]
      . assert(identity)

      test(m"The certificate is version 3 with the given serial number"):
        certificate().asn1 match
          case Asn1.Sequence(List(Asn1.Sequence(tbs), _, _)) => tbs.keep(2)
          case _                                             => Nil
      . assert(_ == List(Asn1.Tagged(0, true, Asn1.Integer(BigInt(2))), Asn1.Integer(BigInt(1))))

      test(m"The signature verifies against the certificate's own public key"):
        val key = PrivateKey.generate[Rsa[2048]]()
        val cert = Certificate.selfSigned(subject, key, period, BigInt(7))

        cert.asn1 match
          case Asn1.Sequence(List(tbs, _, Asn1.BitString(bytes, 0))) =>
            key.public.verify(tbs.in[Der].data, Signature[Rsa[2048]](bytes))

          case _ => false
      . assert(identity)

      test(m"An ECDSA certificate is signed and verifies"):
        val key = PrivateKey.generate[Ecdsa[256]]()
        val cert = Certificate.selfSigned(subject, key, period, BigInt(3))

        cert.asn1 match
          case Asn1.Sequence(List(tbs, _, Asn1.BitString(bytes, 0))) =>
            key.public.verify(tbs.in[Der].data, Signature[Ecdsa[256]](bytes))

          case _ => false
      . assert(identity)

      test(m"The signature algorithm identifier matches the digest"):
        val key = PrivateKey.generate[Rsa[2048]]()
        val cert = Certificate.selfSigned(subject, key, period, BigInt(1))

        cert.asn1 match
          case Asn1.Sequence(List(_, algorithm, _)) => algorithm
          case _                                    => Asn1.Null
      . assert(_ == Asn1.Sequence(List(Asn1.ObjectId(List(1, 2, 840, 113549, 1, 1, 11)), Asn1.Null)))

      test(m"A serial number of zero is refused"):
        capture[Certificate.Error]:
          val key = PrivateKey.generate[Rsa[2048]]()
          Certificate.selfSigned(subject, key, period, BigInt(0))
        . reason
      . assert(_ == Certificate.Error.Reason.BadSerialNumber)

      test(m"A validity period that ends before it starts is refused"):
        capture[Certificate.Error]:
          val key = PrivateKey.generate[Rsa[2048]]()
          val backwards = Instant(1_900_000_000_000L) ~ Instant(1_600_000_000_000L)
          Certificate.selfSigned(subject, key, backwards, BigInt(1))
        . reason
      . assert(_ == Certificate.Error.Reason.BadValidity)

      test(m"A distinguished name encodes its attributes in conventional order"):
        val identifiers = Distinguished.sequence(subject) match
          case Asn1.Sequence(elements) => elements.sweep:
            case Asn1.Set(List(Asn1.Sequence(List(Asn1.ObjectId(arcs), _)))) => arcs

          case _ => Nil

        identifiers
      . assert(_ == List(List(2, 5, 4, 6), List(2, 5, 4, 10), List(2, 5, 4, 3)))

      test(m"The JDK checks an ML-DSA certificate's signature"):
        val key = PrivateKey.generate[MlDsa[65]]()
        val certificate0 = Certificate.selfSigned(subject, key, period, BigInt(5))
        parse(certificate0).verify(publicKey(certificate0))
        true
      . assert(identity)

      test(m"An ML-DSA certificate verifies against its own key"):
        val key = PrivateKey.generate[MlDsa[65]]()
        Certificate.selfSigned(subject, key, period, BigInt(5)).verify(key.public)
      . assert(identity)

      test(m"An ML-DSA certificate does not verify against another key"):
        val key = PrivateKey.generate[MlDsa[65]]()
        val other = PrivateKey.generate[MlDsa[65]]()
        Certificate.selfSigned(subject, key, period, BigInt(5)).verify(other.public)
      . assert(!_)

      test(m"An ML-DSA signature algorithm identifier has no parameters"):
        val key = PrivateKey.generate[MlDsa[65]]()

        Certificate.selfSigned(subject, key, period, BigInt(5)).asn1 match
          case Asn1.Sequence(List(_, algorithm, _)) => algorithm
          case _                                    => Asn1.Null
      . assert(_ == Asn1.Sequence(List(Asn1.ObjectId(List(2, 16, 840, 1, 101, 3, 4, 3, 18)))))

      test(m"Each ML-DSA parameter set has its own signature algorithm identifier"):
        def last(certificate: Certificate): Optional[Int] = certificate.asn1 match
          case Asn1.Sequence(List(_, Asn1.Sequence(List(Asn1.ObjectId(arcs))), _)) => arcs.last
          case _                                                                 => Unset

        val level44 = PrivateKey.generate[MlDsa[44]]()
        val level65 = PrivateKey.generate[MlDsa[65]]()
        val level87 = PrivateKey.generate[MlDsa[87]]()

        List
         ( last(Certificate.selfSigned(subject, level44, period, BigInt(1))),
           last(Certificate.selfSigned(subject, level65, period, BigInt(1))),
           last(Certificate.selfSigned(subject, level87, period, BigInt(1))) )
      . assert(_ == List(17, 18, 19))

      // A service whose identity is an ML-DSA key, but whose TLS stack cannot handshake with one,
      // presents a certificate over a classical key that the ML-DSA key issued, together with the
      // ML-DSA key's own self-signed certificate.
      val issuerName =
        Distinguished(commonName = t"swarm.example.com", organization = t"Propensive")

      def chain(): (PrivateKey[MlDsa[65]], PrivateKey[Ecdsa[256]], Certificate, Certificate) =
        val root = PrivateKey.generate[MlDsa[65]]()
        val leaf = PrivateKey.generate[Ecdsa[256]]()

        val rootCertificate =
          Certificate.selfSigned(issuerName, root, period, BigInt(1), authority = true)

        val leafCertificate =
          Certificate.issued
            ( subject, leaf.public, issuerName, root, period, BigInt(2),
              alternatives = List(t"localhost") )

        (root, leaf, rootCertificate, leafCertificate)

      test(m"A certificate issued by an ML-DSA key over an ECDSA key verifies"):
        val (root, _, _, leaf) = chain()
        leaf.verify(root.public)
      . assert(identity)

      test(m"The JDK checks an ML-DSA-issued certificate against the root"):
        val (_, _, root, leaf) = chain()
        parse(leaf).verify(publicKey(root))
        true
      . assert(identity)

      test(m"An issued certificate names its issuer and carries the subject's key"):
        val (_, _, root, leaf) = chain()
        val parsed = parse(leaf)

        ( parsed.getIssuerX500Principal == parse(root).getSubjectX500Principal,
          parsed.getSubjectX500Principal == parse(certificate()).getSubjectX500Principal,
          parsed.getPublicKey.nn.getAlgorithm.nn.tt,
          parsed.getSigAlgName.nn.tt )
      . assert(_ == (true, true, t"EC", t"ML-DSA-65"))

      test(m"An issued certificate's authority key identifier is the issuer's"):
        val (_, _, root, leaf) = chain()

        // Each extension value is the DER of the extension wrapped in an OCTET STRING; inside, the
        // subject key identifier is a bare OCTET STRING and the authority key identifier a
        // SEQUENCE whose `[0]` element is the same twenty bytes.
        def extension(certificate: jsc.X509Certificate, identifier: String): Data =
          // [java-boundary] fresh array returned by the JDK, retained by nobody else
          val value = Array.unsafeFrozen(certificate.getExtensionValue(identifier).nn)

          Der(value).as[Asn1] match
            case Asn1.OctetString(bytes) => Der(bytes).as[Asn1] match
              case Asn1.OctetString(subject)                                  => subject
              case Asn1.Sequence(List(Asn1.Unknown(2, 0, false, authority))) => authority
              case _                                                          => Data()

            case _ => Data()

        ( extension(parse(leaf), "2.5.29.35").serialize[Hex],
          extension(parse(root), "2.5.29.14").serialize[Hex] )
      . assert { case (authority, subject) => authority == subject && authority.length == 40 }

      test(m"A tampered issued certificate fails against the issuer's key"):
        val (root, _, _, leaf) = chain()
        val data = leaf.in[Der].data
        val last = data.length - 1
        val bytes = Array.allocate[Byte](data.length)
        bytes.place(data)
        bytes(last) = (bytes.readUnchecked(last) ^ 0xff.toByte).toByte
        Certificate(Der(Array.freeze(bytes)).as[Asn1]).verify(root.public)
      . assert(!_)

      test(m"An issued certificate round-trips through DER byte-exactly"):
        val (_, _, _, leaf) = chain()
        val der = leaf.in[Der]
        der.as[Certificate].in[Der] == der
      . assert(identity)

      // Whether the JDK's TLS will present, and accept, an ECDSA leaf that an ML-DSA key issued:
      // SunJSSE has no ML-DSA signature scheme, so it cannot handshake with an ML-DSA key, but
      // the certificate signature is never used in the handshake itself. The `SunX509` key manager
      // chooses its entry by key type alone (the `PKIX` one checks every signature in the chain
      // against the client's advertised schemes, and would refuse), and an
      // `X509ExtendedTrustManager` is consulted directly, where a plain `X509TrustManager` is
      // wrapped in the same check.
      test(m"The JDK's TLS presents and accepts an ECDSA chain that an ML-DSA key issued"):
        import java.util.concurrent as juc
        import javax.net.ssl as jns

        val (_, leaf, rootCertificate, leafCertificate) = chain()
        val password = "swarm".toCharArray.nn

        val privateKey =
          js.KeyFactory.getInstance("EC").nn.generatePrivate
            ( js.spec.PKCS8EncodedKeySpec(Array.unsafeJvm(leaf.pem(Divulgence).data)) )

        val keystore = js.KeyStore.getInstance("PKCS12").nn
        keystore.load(null, null)

        val certificates: scala.Array[jsc.Certificate | Null] =
          scala.Array(parse(leafCertificate), parse(rootCertificate))

        keystore.setKeyEntry("swarm", privateKey, password, certificates)

        val keyManagers = jns.KeyManagerFactory.getInstance("SunX509").nn
        keyManagers.init(keystore, password)
        val serverContext = jns.SSLContext.getInstance("TLS").nn
        serverContext.init(keyManagers.getKeyManagers, null, null)

        type Chain = scala.Array[jsc.X509Certificate | Null] | Null
        @volatile var presented: Chain = null

        val trustManager = new jns.X509ExtendedTrustManager:
          def getAcceptedIssuers: Chain = scala.Array.empty[jsc.X509Certificate | Null]
          def checkClientTrusted(chain: Chain, kind: String | Null): Unit = ()
          def checkServerTrusted(chain: Chain, kind: String | Null): Unit = presented = chain

          def checkClientTrusted(chain: Chain, kind: String | Null, socket: java.net.Socket | Null)
          :   Unit = ()

          def checkServerTrusted(chain: Chain, kind: String | Null, socket: java.net.Socket | Null)
          :   Unit = presented = chain

          def checkClientTrusted(chain: Chain, kind: String | Null, engine: jns.SSLEngine | Null)
          :   Unit = ()

          def checkServerTrusted(chain: Chain, kind: String | Null, engine: jns.SSLEngine | Null)
          :   Unit = presented = chain

        val clientContext = jns.SSLContext.getInstance("TLS").nn
        clientContext.init(null, scala.Array(trustManager), js.SecureRandom())

        val loopback = java.net.InetAddress.getLoopbackAddress

        val serverSocket =
          serverContext.getServerSocketFactory.nn.createServerSocket(0, 1, loopback).nn
          . asInstanceOf[jns.SSLServerSocket]

        serverSocket.setSoTimeout(10000)
        val failure = juc.CompletableFuture[Text]()

        val runnable: Runnable = () =>
          try
            val socket = serverSocket.accept().nn.asInstanceOf[jns.SSLSocket]
            try
              socket.setSoTimeout(10000)
              socket.startHandshake()
              socket.getInputStream.nn.read()
              failure.complete(t"")
            finally socket.close()
          catch case error: Exception => failure.complete(error.toString.tt)

        val server = java.lang.Thread(runnable)
        server.start()

        val outcome: Text =
          try
            val socket =
              clientContext.getSocketFactory.nn.createSocket(loopback, serverSocket.getLocalPort).nn
              . asInstanceOf[jns.SSLSocket]

            try
              socket.setSoTimeout(10000)
              socket.startHandshake()
              socket.getOutputStream.nn.write(1)
              socket.getOutputStream.nn.flush()
              failure.get(10, juc.TimeUnit.SECONDS).nn
            finally socket.close()
          catch case error: Exception => error.toString.tt
          finally
            serverSocket.close()
            server.join(10000)

        val chainOk = presented match
          case null => t"no chain was presented"
          case chain =>
            if chain.length != 2 then t"chain of ${chain.length}" else
              val leaf0 = chain(0).nn
              val root0 = chain(1).nn
              leaf0.verify(root0.getPublicKey)
              // [java-boundary] fresh array returned by the JDK, retained by nobody else
              val leafDer = Array.unsafeFrozen(leaf0.getEncoded.nn).serialize[Hex]
              val expected = leafCertificate.in[Der].data.serialize[Hex]
              if leafDer == expected then t"" else t"a different leaf"

        (outcome, chainOk)
      . assert(_ == (t"", t""))

    suite(m"ASN.1 and DER"):
      // DER is canonical, so `encode` is the equality of record: the byte-carrying cases of `Asn1`
      // are case classes over `Array[Byte]`, whose synthesized `equals` compares arrays by
      // identity. Every assertion below therefore compares hexadecimal encodings.
      def der(value: Asn1): Text = value.in[Der].data.serialize[Hex]
      def decode(hex: Text): Asn1 = Der(hex.deserialize[Hex]).as[Asn1]
      def roundtrip(hex: Text): Text = hex.deserialize[Hex].read[Der].as[Asn1].in[Der].data.serialize[Hex]
      def reason(hex: Text): Asn1.Error.Reason = capture[Asn1.Error](decode(hex)).reason

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
        List(der(Asn1.OctetString(Array[Byte](1, 2, 3, 4, 5))),
            der(Asn1.BitString(Array[Byte](0x6e, 0x5d, 0xc0.toByte), 6)),
            der(Asn1.BitString(Array[Byte](), 0)))
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
        val two = Asn1.OctetString(Array[Byte](2.toByte))
        val one = Asn1.OctetString(Array[Byte](1.toByte))

        List(der(Asn1.Set(List(two, one))),
            der(Asn1.Set(List(Asn1.Integer(BigInt(256)), Asn1.Integer(BigInt(1))))))
      . assert(_ == List(t"3106040101040102", t"310702010102020100"))

      test(m"Context tags encode explicitly, implicitly and in high-tag form"):
        List(der(Asn1.Tagged(0, true, Asn1.Integer(BigInt(2)))),
            der(Asn1.Tagged(0, false, Asn1.OctetString(Array[Byte](0xab.toByte)))),
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
      . assert(_ == Asn1.Error.Reason.IndefiniteLength(1))

      test(m"Overlong lengths and tags are rejected"):
        List(reason(t"018101FF"), reason(t"1F1E00"))
      . assert(_ == List(Asn1.Error.Reason.NonMinimalLength(1), Asn1.Error.Reason.NonMinimalTag(1)))

      test(m"A length needing more than four bytes is rejected"):
        reason(t"048501010101010101")
      . assert(_ == Asn1.Error.Reason.Overflow(1))

      test(m"Non-minimal and empty integers are rejected"):
        List(reason(t"02020001"), reason(t"0200"))
      . assert(_ == List(Asn1.Error.Reason.NonMinimalInteger(0),
          Asn1.Error.Reason.EmptyInteger(0)))

      test(m"A boolean other than 0x00 or 0xFF is rejected"):
        reason(t"010101")
      . assert(_ == Asn1.Error.Reason.BadBoolean(0, 1))

      test(m"A null with content is rejected"):
        reason(t"050100")
      . assert(_ == Asn1.Error.Reason.BadLength(0, 5, 1))

      test(m"Invalid bit-string padding is rejected"):
        List(reason(t"03020801"), reason(t"03020103"))
      . assert(_ == List(Asn1.Error.Reason.BadUnusedBits(0, 8),
          Asn1.Error.Reason.BadUnusedBits(0, 1)))

      test(m"Constructed strings and primitive sequences are rejected"):
        List(reason(t"240404020102"), reason(t"1003020101"))
      . assert(_ == List(Asn1.Error.Reason.NotPrimitive(0, 4),
          Asn1.Error.Reason.NotConstructed(0, 16)))

      test(m"Malformed object identifiers are rejected"):
        List(reason(t"06022A80"), reason(t"06062A8FFFFFFF7F"))
      . assert(_ == List(Asn1.Error.Reason.BadOid(3), Asn1.Error.Reason.OidArcOverflow(3)))

      test(m"An unsorted set is rejected"):
        reason(t"3106040102040101")
      . assert(_ == Asn1.Error.Reason.UnsortedSet(5))

      test(m"An impossible date is rejected"):
        reason(t"170D3730303233303030303030305A")
      . assert(_ == Asn1.Error.Reason.BadTime(0))

      test(m"Truncated input and trailing bytes are rejected"):
        List(reason(t"0101"), reason(t"05000500"))
      . assert(_ == List(Asn1.Error.Reason.Truncated(2), Asn1.Error.Reason.Trailing(2)))

      test(m"The reserved tag number zero is rejected"):
        reason(t"0000")
      . assert(_ == Asn1.Error.Reason.ReservedTag(0))

      test(m"A real X.509 certificate round-trips byte-exactly"):
        val value: Asn1 = certificate.read[Asn1 in Pem]
        value.in[Der] == certificate.read[Der in Pem]
      . assert(identity)

      test(m"A real PKCS#10 request round-trips byte-exactly"):
        val pem = request.read[Pem]
        pem.as[Der].as[Asn1].in[Der] == pem.as[Der]
      . assert(identity)

      test(m"A DER document re-armors as PEM"):
        val value: Asn1 = certificate.read[Asn1 in Pem]
        Pem(Pem.Label.Certificate, value.in[Der]).serialize
      . assert(_ == certificate.trim)

    // A missing `Inspectable` is never a compile error — `derived` always succeeds and
    // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
    // only be held in place by asserting on the renderings.
    suite(m"Native-rendering coverage"):
      test(m"enigmatic's types inspect natively"):
        val key = PrivateKey.generate[Rsa[2048]]()

        Inspectable.fallbacks
         ( (Asn1.Integer(BigInt(42)): Asn1).in[Der].inspect,
           pangram.hmac[Sha2[256]](t"a key".in[Data]).inspect,
           Password(t"secret").inspect,
           key.inspect,
           key.public.inspect,
           key.sign(pangram).inspect,
           certificate.read[Pem].inspect,
           Certificate(certificate.read[Asn1 in Pem]).inspect )
      . assert(_ == Nil)

      test(m"A DER document inspects as its full hexadecimal"):
        (Asn1.Integer(BigInt(42)): Asn1).in[Der].inspect
      . assert(_ == t"Der(02012a)")

      test(m"A password never reveals its cleartext"):
        Password(t"secret").inspect
      . assert(_ == t"Password(\u2022\u2022\u2022)")

      test(m"A private key never reveals its key material"):
        PrivateKey.generate[Rsa[2048]]().inspect
      . assert(_ == t"PrivateKey(\u2022\u2022\u2022)")

