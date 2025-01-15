/*
    Gastronomy, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gastronomy

import anticipation.*
import contingency.*, strategies.throwUnsafely
import gossamer.*
import hieroglyph.*, charDecoders.utf8, charEncoders.utf8, textSanitizers.skip
import monotonous.*
import probably.*
import rudiments.*
import spectacular.*

import alphabets.hex.upperCase

object Tests extends Suite(t"Gastronomy tests"):

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
    test(t"Sha256, Hex"):
      t"Hello world".digest[Sha2[256]].serialize[Hex]
    .assert(_ == t"64EC88CA00B268E5BA1A35678A1B5316D212F4F366B2477232534A8AECA37F3C")

    test(t"Md5, Base64"):
      import alphabets.base64.standard
      t"Hello world".digest[Md5].serialize[Base64]
    .assert(_ == t"PiWWCnnbxptnTNTsZ6csYg==")

    test(t"Sha1, Base64Url"):
      import alphabets.base64.url
      println(t"Hello world".digest[Sha1].serialize[Base64])
      t"Hello world".digest[Sha1].bytes.serialize[Base64]
    .assert(_ == t"e1AsOh9IyGCa4hLN-2Od7jlnP14")

    test(t"Sha384, Base64"):
      import alphabets.base64.standard
      t"Hello world".digest[Sha2[384]].serialize[Base64]
    .assert(_ == t"kgOwxEOf0eauWHiGYze3xTKs1tkmAVDIAxjoq4wnzjMBifjflPuJDfHSmP82Bifh")

    test(t"Sha512, Base64"):
      import alphabets.base64.standard
      t"Hello world".digest[Sha2[512]].serialize[Base64]
    .assert(_ == t"t/eDuu2Cl/DbkXRiGE/08I5pwtXl95qUJgD5cl9Yzh8pwYE5v4CwbA//K900c4RS7PQMSIwip+PYDN9vnBwNRw==")

    test(t"Encode to Binary"):
      IArray[Byte](1, 2, 3, 4).serialize[Binary]
    .assert(_ == t"00000001000000100000001100000100")
