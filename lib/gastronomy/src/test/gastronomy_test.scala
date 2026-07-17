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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package gastronomy

import soundness.*

import providers.javaStdlibProvider, providers.soundnessProvider
import crypto.permitDisallowedCrypto   // the suite exercises MD5 and SHA-1

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

    test(m"Long digest covers the low nibble"):
      0L.digest[Sha2[256]].serialize[Hex] != 0xfL.digest[Sha2[256]].serialize[Hex]
    . assert(identity(_))

    test(m"Long digest covers the high nibble"):
      0L.digest[Sha2[256]].serialize[Hex]
      != 0xf000000000000000L.digest[Sha2[256]].serialize[Hex]
    . assert(identity(_))

    test(m"Blake3 via Hash typeclass, empty input"):
      IArray[Byte]().digest[Blake3].serialize[Hex]
    . assert: digest =>
        digest == t"AF1349B9F5F9A1A6A0404DEA36DCC9499BCB25C9ADC112B7CC9A93CAE41F3262"

    suite(m"Blake3 official test vectors"):
      val key: IArray[Byte] = Blake3TestVectors.Key.getBytes("UTF-8").nn.immutable(using Unsafe)
      val context: Text = Blake3TestVectors.ContextString.tt

      Blake3TestVectors.cases.each: vector =>
        val input = IArray.tabulate(vector.inputLen)(i => (i % 251).toByte)
        val expectedHash      = vector.hash.toUpperCase.nn.tt
        val expectedKeyed     = vector.keyedHash.toUpperCase.nn.tt
        val expectedDerived   = vector.deriveKey.toUpperCase.nn.tt

        test(m"hash, inputLen=${vector.inputLen}"):
          Blake3.hashOf(input, 131).serialize[Hex]
        . assert(_ == expectedHash)

        test(m"keyedHash, inputLen=${vector.inputLen}"):
          Blake3.keyedHash(key, input, 131).serialize[Hex]
        . assert(_ == expectedKeyed)

        test(m"deriveKey, inputLen=${vector.inputLen}"):
          Blake3.deriveKey(context, input, 131).serialize[Hex]
        . assert(_ == expectedDerived)

    // The pure-Scala hash implementations (used off the JVM, where `MessageDigest` is absent) are
    // exercised directly here against the NIST/RFC "abc" vectors and cross-validated against the
    // JDK for random inputs across the block-boundary sizes.
    suite(m"Pure hash implementations"):
      def hex(digestion: Digestion, message: Text): Text =
        digestion.append(message.s.getBytes("UTF-8").nn.immutable(using Unsafe))
        digestion.digest().serialize[Hex]

      test(m"pure SHA-256 of \"abc\""):
        hex(PureHashes.sha2(256), t"abc")
      . assert(_ == t"BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD")

      test(m"pure SHA-224 of \"abc\""):
        hex(PureHashes.sha2(224), t"abc")
      . assert(_ == t"23097D223405D8228642A477BDA255B32AADBCE4BDA0B3F7E36C9DA7")

      test(m"pure SHA-384 of \"abc\""):
        hex(PureHashes.sha2(384), t"abc")
      . assert: digest =>
          digest == t"CB00753F45A35E8BB5A03D699AC65007272C32AB0EDED1631A8B605A43FF5BED"
              + t"8086072BA1E7CC2358BAECA134C825A7"

      test(m"pure SHA-512 of \"abc\""):
        hex(PureHashes.sha2(512), t"abc")
      . assert: digest =>
          digest == t"DDAF35A193617ABACC417349AE20413112E6FA4E89A97EA20A9EEEE64B55D39A"
              + t"2192992A274FC1A836BA3C23A3FEEBBD454D4423643CE80E2A9AC94FA54CA49F"

      test(m"pure SHA-1 of \"abc\""):
        hex(PureHashes.sha1, t"abc")
      . assert(_ == t"A9993E364706816ABA3E25717850C26C9CD0D89D")

      test(m"pure MD5 of \"abc\""):
        hex(PureHashes.md5, t"abc")
      . assert(_ == t"900150983CD24FB0D6963F7D28E17F72")

      // Random inputs at and around the 55/56/63/64/111/112/127/128-byte padding boundaries.
      val sizes = List(0, 1, 55, 56, 57, 63, 64, 65, 111, 112, 113, 127, 128, 129, 1000)

      sizes.each: size =>
        val data: Data = IArray.tabulate(size)(i => ((i*31 + 7) & 0xff).toByte)

        def jdk(name: Text): Text =
          val md = java.security.MessageDigest.getInstance(name.s).nn
          md.digest(data.mutable(using Unsafe)).nn.immutable(using Unsafe).serialize[Hex]

        def pureHex(digestion: Digestion): Text =
          digestion.append(data)
          digestion.digest().serialize[Hex]

        test(m"pure SHA-256 matches the JDK, size=$size"):
          pureHex(PureHashes.sha2(256))
        . assert(_ == jdk(t"SHA-256"))

        test(m"pure SHA-512 matches the JDK, size=$size"):
          pureHex(PureHashes.sha2(512))
        . assert(_ == jdk(t"SHA-512"))

        test(m"pure SHA-1 matches the JDK, size=$size"):
          pureHex(PureHashes.sha1)
        . assert(_ == jdk(t"SHA-1"))

        test(m"pure MD5 matches the JDK, size=$size"):
          pureHex(PureHashes.md5)
        . assert(_ == jdk(t"MD5"))
