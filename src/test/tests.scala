/*
    Gastronomy, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import probably.*
import rudiments.*
import anticipation.*
import gossamer.*
import perforate.*, errorHandlers.throwUnsafely
import spectacular.*
import hieroglyph.*, charDecoders.utf8, charEncoders.utf8, badEncodingHandlers.skip

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
      t"Hello world".digest[Sha2[256]].encodeAs[Hex]
    .assert(_ == t"64EC88CA00B268E5BA1A35678A1B5316D212F4F366B2477232534A8AECA37F3C")

    test(t"Md5, Base64"):
      t"Hello world".digest[Md5].encodeAs[Base64]
    .assert(_ == t"PiWWCnnbxptnTNTsZ6csYg==")

    test(t"Sha1, Base64Url"):
      t"Hello world".digest[Sha1].encodeAs[Base64Url]
    .assert(_ == t"e1AsOh9IyGCa4hLN-2Od7jlnP14")

    test(t"Sha384, Base64"):
      t"Hello world".digest[Sha2[384]].encodeAs[Base64]
    .assert(_ == t"kgOwxEOf0eauWHiGYze3xTKs1tkmAVDIAxjoq4wnzjMBifjflPuJDfHSmP82Bifh")
    
    test(t"Sha512, Base64"):
      t"Hello world".digest[Sha2[512]].encodeAs[Base64]
    .assert(_ == t"t/eDuu2Cl/DbkXRiGE/08I5pwtXl95qUJgD5cl9Yzh8pwYE5v4CwbA//K900c4RS7PQMSIwip+PYDN9vnBwNRw==")

    test(t"Encode to Binary"):
      IArray[Byte](1, 2, 3, 4).encodeAs[Binary]
    .assert(_ == t"00000001000000100000001100000100")

    test(t"Extract PEM message type"):
      val example = t"""
        |-----BEGIN EXAMPLE-----
        |MIIB9TCCAWACAQAwgbgxGTAXBgNVBAoMEFF1b1ZhZGlzIExpbWl0ZWQxHDAaBgNV
        |-----END EXAMPLE-----
        """.s.stripMargin.show
      
      Pem.parse(example).kind
    .assert(_ == t"EXAMPLE")

    test(t"Decode PEM certificate"):
      Pem.parse(request).data.digest[Md5].encodeAs[Base64]
    .assert(_ == t"iMwRdyDFStqq08vqjPbzYw==")
  
    test(t"PEM roundtrip"):
      Pem.parse(request).serialize
    .assert(_ == request.trim)

    test(t"RSA roundtrip"):
      val privateKey: PrivateKey[Rsa[1024]] = PrivateKey.generate[Rsa[1024]]()
      val message: MessageData[Rsa[1024]] = privateKey.public.encrypt(t"Hello world")
      privateKey.decrypt[Text](message.bytes)
    .assert(_ == t"Hello world")
    
    test(t"AES roundtrip"):
      val key: SymmetricKey[Aes[256]] = SymmetricKey.generate[Aes[256]]()
      val message = key.encrypt(t"Hello world")
      key.decrypt[Text](message.bytes)
    .assert(_ == t"Hello world")

    test(t"Sign some data with DSA"):
      val privateKey: PrivateKey[Dsa[1024]] = PrivateKey.generate[Dsa[1024]]()
      val message = t"Hello world"
      val signature = privateKey.sign(message)
      privateKey.public.verify(message, signature)
    .assert(identity)

    test(t"Check bad signature"):
      val privateKey: PrivateKey[Dsa[1024]] = PrivateKey.generate[Dsa[1024]]()
      val message = t"Hello world"
      val signature = privateKey.sign(t"Something else")
      privateKey.public.verify(message, signature)
    .assert(!identity(_))

    test(t"MD5 HMAC"):
      pangram.hmac[Md5](t"key".bytes).encodeAs[Hex]
    .check(_ == t"80070713463E7749B90C2DC24911E275")
    
    test(t"SHA1 HMAC"):
      pangram.hmac[Sha1](t"key".bytes).encodeAs[Hex]
    .assert(_ == t"DE7C9B85B8B78AA6BC8A7A36F70A90701C9DB4D9")

    test(t"SHA256 HMAC"):
      pangram.hmac[Sha2[256]](t"key".bytes).encodeAs[Hex]
    .assert(_ == t"F7BC83F430538424B13298E6AA6FB143EF4D59A14946175997479DBC2D1A3CD8")
    
    test(t"SHA384 HMAC"):
      pangram.hmac[Sha2[384]](t"key".bytes).encodeAs[Base64]
    .assert(_ == t"1/RyfiwLOa4PHkDMlvYCQtW3gBhBzqb8WSxdPhrlBwBYKpbPNeHlVJlf5OAzgcI3")
    
    test(t"SHA512 HMAC"):
      pangram.hmac[Sha2[512]](t"key".bytes).encodeAs[Base64]
    .assert(_ == t"tCrwkFe6weLUFwjkipAuCbX/fxKrQopP6GZTxz3SSPuC+UilSfe3kaW0GRXuTR7Dk1NX5OIxclDQNyr6Lr7rOg==")
