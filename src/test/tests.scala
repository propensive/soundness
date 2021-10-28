/*
    Gastronomy, version 0.9.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import eucalyptus.*

given Log(Everything |-> Stdout)

import unsafeExceptions.canThrowAny

object Tests extends Suite(str"Gastronomy tests"):
 
  val request: Txt = str"""
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

  val pangram: Txt = str"The quick brown fox jumps over the lazy dog"

  def run(using Runner): Unit = {
    test(str"Sha256, Hex") {
      str"Hello world".digest[Sha2[256]].encode[Hex]
    }.check(_ == str"64EC88CA00B268E5BA1A35678A1B5316D212F4F366B2477232534A8AECA37F3C")

    test(str"Md5, Base64") {
      str"Hello world".digest[Md5].encode[Base64]
    }.check(_ == str"PiWWCnnbxptnTNTsZ6csYg==")

    test(str"Sha1, Base64Url") {
      str"Hello world".digest[Sha1].encode[Base64Url]
    }.check(_ == str"e1AsOh9IyGCa4hLN-2Od7jlnP14")

    test(str"Sha384, Base64") {
      str"Hello world".digest[Sha2[384]].encode[Base64]
    }.check(_ == str"kgOwxEOf0eauWHiGYze3xTKs1tkmAVDIAxjoq4wnzjMBifjflPuJDfHSmP82Bifh")
    
    test(str"Sha512, Base64") {
      str"Hello world".digest[Sha2[512]].encode[Base64]
    }.check(_ == str"t/eDuu2Cl/DbkXRiGE/08I5pwtXl95qUJgD5cl9Yzh8pwYE5v4CwbA//K900c4RS7PQMSIwip+PYDN9vnBwNRw==")

    test(str"Encode to Binary") {
      IArray[Byte](1, 2, 3, 4).encode[Binary]
    }.check(_ == str"00000001000000100000001100000100")

    test(str"Extract PEM message type") {
      val example = str"""
        |-----BEGIN EXAMPLE-----
        |MIIB9TCCAWACAQAwgbgxGTAXBgNVBAoMEFF1b1ZhZGlzIExpbWl0ZWQxHDAaBgNV
        |-----END EXAMPLE-----
        """.s.stripMargin.show
      
      Pem.parse(example).kind
    }.check(_ == str"EXAMPLE")

    test(str"Decode PEM certificate") {
      Pem.parse(request).data.digest[Md5].encode[Base64]
    }.check(_ == str"iMwRdyDFStqq08vqjPbzYw==")
  
    test(str"PEM roundtrip") {
      Pem.parse(request).serialize
    }.assert(_ == request.trim)

    test(str"RSA roundtrip") {
      val privateKey: PrivateKey[Rsa[1024]] = PrivateKey.generate[Rsa[1024]]()
      val message: Message[Rsa[1024]] = privateKey.public.encrypt(str"Hello world")
      privateKey.decrypt[Txt](message.bytes)
    }.check(_ == str"Hello world")
    
    test(str"AES roundtrip") {
      val key: SymmetricKey[Aes[256]] = SymmetricKey.generate[Aes[256]]()
      val message = key.encrypt(str"Hello world")
      key.decrypt[Txt](message.bytes)
    }.check(_ == str"Hello world")

    test(str"Sign some data with DSA") {
      val privateKey: PrivateKey[Dsa[1024]] = PrivateKey.generate[Dsa[1024]]()
      val message = str"Hello world"
      val signature = privateKey.sign(message)
      privateKey.public.verify(message, signature)
    }.assert(identity)

    test(str"Check bad signature") {
      val privateKey: PrivateKey[Dsa[1024]] = PrivateKey.generate[Dsa[1024]]()
      val message = str"Hello world"
      val signature = privateKey.sign(str"Something else")
      privateKey.public.verify(message, signature)
    }.assert(!identity(_))

    test(str"MD5 HMAC") {
      pangram.hmac[Md5](str"key".bytes).encode[Hex]
    }.check(_ == str"80070713463E7749B90C2DC24911E275")
    
    test(str"SHA1 HMAC") {
      pangram.hmac[Sha1](str"key".bytes).encode[Hex]
    }.check(_ == str"DE7C9B85B8B78AA6BC8A7A36F70A90701C9DB4D9")

    test(str"SHA256 HMAC") {
      pangram.hmac[Sha2[256]](str"key".bytes).encode[Hex]
    }.check(_ == str"F7BC83F430538424B13298E6AA6FB143EF4D59A14946175997479DBC2D1A3CD8")
    
    test(str"SHA384 HMAC") {
      pangram.hmac[Sha2[384]](str"key".bytes).encode[Base64]
    }.check(_ == str"1/RyfiwLOa4PHkDMlvYCQtW3gBhBzqb8WSxdPhrlBwBYKpbPNeHlVJlf5OAzgcI3")
    
    test(str"SHA512 HMAC") {
      pangram.hmac[Sha2[512]](str"key".bytes).encode[Base64]
    }.check(_ == str"tCrwkFe6weLUFwjkipAuCbX/fxKrQopP6GZTxz3SSPuC+UilSfe3kaW0GRXuTR7Dk1NX5OIxclDQNyr6Lr7rOg==")
  }
