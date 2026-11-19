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
package sigillate

import soundness.*

import strategies.throwUnsafely
import charsets.utf8Charset, codepages.utf8Codepage, textSanitizers.skipSanitizer
import gossamer.textDecodable
import errorDiagnostics.stackTracesDiagnostics
import providers.javaBaseProvider
import cloaks.heapCloak
import formatting.compactJsonFormatting
import dysasymptotics.linearSize

case class Role(part: Text, play: Text)

object Tests extends Suite("jose", m"Sigillate tests"):
  // The claims RFC 7515's examples sign, line breaks and all.
  val joe: Text = t"{\"iss\":\"joe\",\r\n \"exp\":1300819380,\r\n \"http://example.com/is_root\":true}"

  val wit: Text = t"The true sign of intelligence is not knowledge but imagination."
  val farewell: Text = t"Live long and prosper."
  val sonnet: Text = t"Shall I compare thee to a summer's day?"

  def jwk(text: Text): Jwk = text.read[Json].as[Jwk]
  def keys(text: Text): Jwks = text.read[Json].as[Jwks]

  def run(): Unit =
    suite(m"JWS"):
      test(m"RFC 7515 A.1: the HS256 example verifies"):
        val key = jwk(Vectors.hs256Key).symmetricKey[HmacCipher[Sha2[256]]]
        Vectors.hs256Jws.as[Jws].verify(key).read[Text]
      . assert(_ == joe)

      test(m"RFC 7515 A.2: the RS256 example verifies"):
        val key = jwk(Vectors.rs256Key).publicKey[Rsa[2048]]
        Vectors.rs256Jws.as[Jws].verify(key).read[Text]
      . assert(_ == joe)

      test(m"RFC 7515 A.2: signing reproduces the RS256 example"):
        val key = jwk(Vectors.rs256Key).privateKey[Rsa[2048]]
        Jws(joe, key).in[Text]
      . assert(_ == Vectors.rs256Jws)

      test(m"RFC 7515 A.3: the ES256 example verifies"):
        val key = jwk(Vectors.es256Key).publicKey[Ecdsa[256]]
        Vectors.es256Jws.as[Jws].verify(key).read[Text]
      . assert(_ == joe)

      test(m"RFC 7515 A.4: the ES512 example verifies"):
        val key = jwk(Vectors.es512Key).publicKey[Ecdsa[521]]
        Vectors.es512Jws.as[Jws].verify(key).read[Text]
      . assert(_ == t"Payload")

      test(m"RFC 7515 A.5: an unsecured JWS gives its payload when asked"):
        Vectors.unsecuredJws.as[Jws].payload(Jose.Unsecured).read[Text]
      . assert(_ == joe)

      test(m"RFC 7515 A.5: an unsecured JWS does not verify under a key"):
        val key = jwk(Vectors.hs256Key).symmetricKey[HmacCipher[Sha2[256]]]
        capture[Jose.Error](Vectors.unsecuredJws.as[Jws].verify(key)).reason
      . assert(_ == Jose.Error.Reason.Mismatch(t"HS256", t"none"))

      test(m"A signed JWS is not an unsecured one"):
        capture[Jose.Error](Vectors.hs256Jws.as[Jws].payload(Jose.Unsecured)).reason
      . assert(_ == Jose.Error.Reason.Secured)

      test(m"RFC 7515 A.6: the general serialization has two signatures"):
        Jws.read(Vectors.generalJws.read[Json]).size
      . assert(_ == 2)

      test(m"RFC 7515 A.6: the first signature verifies under the RSA key"):
        val key = jwk(Vectors.rs256Key).publicKey[Rsa[2048]]
        Jws.read(Vectors.generalJws.read[Json]).head.let(_.verify(key).read[Text])
      . assert(_ == joe)

      test(m"RFC 7515 A.6: the second signature verifies under the EC key"):
        val key = jwk(Vectors.es256Key).publicKey[Ecdsa[256]]
        Jws.read(Vectors.generalJws.read[Json]).last.let(_.verify(key).read[Text])
      . assert(_ == joe)

      test(m"RFC 7515 A.6: the general serialization is written back unchanged"):
        Jws.general(Jws.read(Vectors.generalJws.read[Json])).show
      . assert(_ == Vectors.generalJws)

      test(m"RFC 7515 A.7: the flattened serialization verifies"):
        val key = jwk(Vectors.es256Key).publicKey[Ecdsa[256]]
        Jws.read(Vectors.flattenedJws.read[Json]).head.let(_.verify(key).read[Text])
      . assert(_ == joe)

      test(m"RFC 7515 A.7: the flattened serialization keeps its unprotected header"):
        Jws.read(Vectors.flattenedJws.read[Json]).head.let(_.unprotected.let(_.keyId))
      . assert(_ == t"e9bc097a-ce51-4036-9562-d2ade882db0d")

      test(m"RFC 7515 E: a critical parameter that is not understood is refused"):
        val token =
          t"eyJhbGciOiJub25lIiwNCiAiY3JpdCI6WyJodHRwOi8vZXhhbXBsZS5jb20vVU5ERUZJTkVEIl0sDQogImh0dHA6Ly9leGFtcGxlLmNvbS9VTkRFRklORUQiOnRydWV9.RkFJTA."

        capture[Jose.Error](token.as[Jws].payload(Jose.Unsecured)).reason
      . assert(_ == Jose.Error.Reason.Critical(t"http://example.com/UNDEFINED"))

      test(m"RFC 8037 A.4: the EdDSA example verifies"):
        val key = jwk(Vectors.ed25519Key).publicKey[Ed25519]
        Vectors.ed25519Jws.as[Jws].verify(key).read[Text]
      . assert(_ == t"Example of Ed25519 signing")

      test(m"RFC 8037 A.4: signing reproduces the EdDSA example"):
        val key = jwk(Vectors.ed25519Key).privateKey[Ed25519]
        Jws(t"Example of Ed25519 signing", key).in[Text]
      . assert(_ == Vectors.ed25519Jws)

      test(m"An RSA public key replayed as an HMAC secret is refused"):
        val secret = SymmetricKey[HmacCipher[Sha2[256]]](jwk(Vectors.rs256Key).publicKey[Rsa[2048]].bytes)
        capture[Jose.Error](Vectors.rs256Jws.as[Jws].verify(secret)).reason
      . assert(_ == Jose.Error.Reason.Mismatch(t"HS256", t"RS256"))

      test(m"A JWS with an altered payload does not verify"):
        val key = jwk(Vectors.hs256Key).symmetricKey[HmacCipher[Sha2[256]]]
        val jws = Vectors.hs256Jws.as[Jws]
        capture[Jose.Error](jws.copy(content = t"eyJpc3MiOiJtYWxsb3J5In0").verify(key)).reason
      . assert(_ == Jose.Error.Reason.BadSignature)

      test(m"A JWS does not verify under another key"):
        val key = SymmetricKey.generate[HmacCipher[Sha2[256]]]()
        capture[Jose.Error](Vectors.hs256Jws.as[Jws].verify(key)).reason
      . assert(_ == Jose.Error.Reason.BadSignature)

      test(m"Text that is not three parts is not a JWS"):
        capture[Jose.Error](t"eyJhbGciOiJIUzI1NiJ9.e30".as[Jws]).reason
      . assert(_ == Jose.Error.Reason.Malformed(t"a compact JWS has three parts"))

      test(m"HS384 and HS512 sign and verify"):
        val key384 = SymmetricKey.generate[HmacCipher[Sha2[384]]]()
        val key512 = SymmetricKey.generate[HmacCipher[Sha2[512]]]()

        List
          ( Jws(sonnet, key384).in[Text].as[Jws].verify(key384).read[Text],
            Jws(sonnet, key512).in[Text].as[Jws].verify(key512).read[Text] )
      . assert(_ == List(sonnet, sonnet))

      test(m"The header names the algorithm of the key"):
        val names =
          List
            ( Jws(sonnet, SymmetricKey.generate[HmacCipher[Sha2[384]]]()).header.algorithm,
              Jws(sonnet, PrivateKey.generate[Ecdsa[384]]()).header.algorithm,
              Jws(sonnet, PrivateKey.generate[RsaPss[2048]]()).header.algorithm,
              Jws(sonnet, PrivateKey.generate[Ed25519]()).header.algorithm )

        names
      . assert(_ == List(t"HS384", t"ES384", t"PS256", t"EdDSA"))

      test(m"ES384 signs and verifies with a 96-byte signature"):
        val key = PrivateKey.generate[Ecdsa[384]]()
        val jws = Jws(sonnet, key).in[Text].as[Jws]
        (jws.signature.length, jws.verify(key.public).read[Text])
      . assert(_ == (96, sonnet))

      test(m"PS256 signs and verifies"):
        val key = PrivateKey.generate[RsaPss[2048]]()
        Jws(sonnet, key).in[Text].as[Jws].verify(key.public).read[Text]
      . assert(_ == sonnet)

      test(m"RS512 is chosen by the signature digest in scope"):
        import signatureDigests.sha512Signature
        val key = PrivateKey.generate[Rsa[2048]]()
        val jws = Jws(sonnet, key)
        (jws.header.algorithm, jws.verify(key.public).read[Text])
      . assert(_ == (t"RS512", sonnet))

      test(m"A PS256 key does not verify an RS256 signature"):
        val key = jwk(Vectors.rs256Key).publicKey[RsaPss[2048]]
        capture[Jose.Error](Vectors.rs256Jws.as[Jws].verify(key)).reason
      . assert(_ == Jose.Error.Reason.Mismatch(t"PS256", t"RS256"))

      test(m"A header's other parameters are kept"):
        val key = SymmetricKey.generate[HmacCipher[Sha2[256]]]()
        val jws = Jws(sonnet, key, Jose.Header(keyId = t"globe", contentType = t"text/plain"))
        val header = jws.in[Text].as[Jws].header
        (header.keyId, header.contentType)
      . assert(_ == (t"globe", t"text/plain"))

    suite(m"JWE"):
      test(m"RFC 7516 A.1: the RSA-OAEP and A256GCM example decrypts"):
        val key = jwk(Vectors.rsaOaepKey).privateKey[RsaOaep[2048, Sha1]]
        Vectors.rsaOaepJwe.as[Jwe].decrypt[AesGcm[256]](key).read[Text]
      . assert(_ == wit)

      test(m"RFC 7516 A.2: the RSA1_5 and A128CBC-HS256 example decrypts"):
        val key = jwk(Vectors.rsa15Key).privateKey[Rsa[2048]]
        Vectors.rsa15Jwe.as[Jwe].decrypt[AesCbcHmac[256]](key).read[Text]
      . assert(_ == farewell)

      test(m"RFC 7516 A.3: the A128KW and A128CBC-HS256 example decrypts"):
        val key = jwk(Vectors.aesKwKey).symmetricKey[AesKeyWrap[128]]
        Vectors.aesKwJwe.as[Jwe].decrypt[AesCbcHmac[256]](key).read[Text]
      . assert(_ == farewell)

      test(m"RFC 7516 A.4: the general serialization has two recipients"):
        Jwe.read(Vectors.generalJwe.read[Json]).size
      . assert(_ == 2)

      test(m"RFC 7516 A.4: the first recipient decrypts with the RSA key"):
        val key = jwk(Vectors.rsa15Key).privateKey[Rsa[2048]]
        Jwe.read(Vectors.generalJwe.read[Json]).head.let(_.decrypt[AesCbcHmac[256]](key).read[Text])
      . assert(_ == farewell)

      test(m"RFC 7516 A.4: the second recipient decrypts with the AES key"):
        val key = jwk(Vectors.aesKwKey).symmetricKey[AesKeyWrap[128]]
        Jwe.read(Vectors.generalJwe.read[Json]).last.let(_.decrypt[AesCbcHmac[256]](key).read[Text])
      . assert(_ == farewell)

      test(m"RFC 7516 A.4: a recipient's key does not open another's entry"):
        val key = jwk(Vectors.aesKwKey).symmetricKey[AesKeyWrap[128]]
        val first = Jwe.read(Vectors.generalJwe.read[Json]).head.or(panic(m"there are two"))
        capture[Jose.Error](first.decrypt[AesCbcHmac[256]](key)).reason
      . assert(_ == Jose.Error.Reason.Mismatch(t"A128KW", t"RSA1_5"))

      test(m"RFC 7516 A.4: the general serialization is written back unchanged"):
        Jwe.general(Jwe.read(Vectors.generalJwe.read[Json])).show
      . assert(_ == Vectors.generalJwe)

      test(m"RFC 7516 A.5: the flattened serialization decrypts"):
        val key = jwk(Vectors.aesKwKey).symmetricKey[AesKeyWrap[128]]
        Jwe.read(Vectors.flattenedJwe.read[Json]).head.let(_.decrypt[AesCbcHmac[256]](key).read[Text])
      . assert(_ == farewell)

      test(m"RFC 7517 C: a key encrypted under a password decrypts"):
        val password = Password(t"Thus from my lips, by yours, my sin is purged.")
        val plaintext = Vectors.passwordJwe.as[Jwe].decrypt[AesCbcHmac[256]](password)
        Jose.parse(plaintext).as[Jwk].keyId
      . assert(_ == t"juliet@capulet.lit")

      test(m"A password of the wrong case does not decrypt"):
        val password = Password(t"thus from my lips, by yours, my sin is purged.")
        capture[Jose.Error](Vectors.passwordJwe.as[Jwe].decrypt[AesCbcHmac[256]](password)).reason
      . assert(_ == Jose.Error.Reason.Undecryptable)

      test(m"The content cipher must be the one the header names"):
        val key = jwk(Vectors.aesKwKey).symmetricKey[AesKeyWrap[128]]
        capture[Jose.Error](Vectors.aesKwJwe.as[Jwe].decrypt[AesGcm[128]](key)).reason
      . assert(_ == Jose.Error.Reason.Mismatch(t"A128GCM", t"A128CBC-HS256"))

      test(m"An altered ciphertext does not decrypt"):
        val key = jwk(Vectors.aesKwKey).symmetricKey[AesKeyWrap[128]]
        val jwe = Vectors.aesKwJwe.as[Jwe]
        val altered = jwe.copy(ciphertext = Array.concat(jwe.ciphertext.skip(1), jwe.ciphertext.keep(1)))
        capture[Jose.Error](altered.decrypt[AesCbcHmac[256]](key)).reason
      . assert(_ == Jose.Error.Reason.Undecryptable)

      test(m"RSA-OAEP-256 with A128GCM round-trips"):
        val key = PrivateKey.generate[RsaOaep[2048, Sha2[256]]]()
        val jwe = Jwe[AesGcm[128]](sonnet, key.public).in[Text].as[Jwe]
        (jwe.header.algorithm, jwe.decrypt[AesGcm[128]](key).read[Text])
      . assert(_ == (t"RSA-OAEP-256", sonnet))

      test(m"A192KW with A192CBC-HS384 round-trips"):
        val key = SymmetricKey.generate[AesKeyWrap[192]]()
        val jwe = Jwe[AesCbcHmac[384]](sonnet, key).in[Text].as[Jwe]
        (jwe.header.algorithm, jwe.header.encryption, jwe.decrypt[AesCbcHmac[384]](key).read[Text])
      . assert(_ == (t"A192KW", t"A192CBC-HS384", sonnet))

      test(m"A256KW with A256CBC-HS512 round-trips"):
        val key = SymmetricKey.generate[AesKeyWrap[256]]()
        Jwe[AesCbcHmac[512]](sonnet, key).in[Text].as[Jwe].decrypt[AesCbcHmac[512]](key).read[Text]
      . assert(_ == sonnet)

      test(m"Direct encryption with an AES-GCM key carries no encrypted key"):
        val key = SymmetricKey.generate[AesGcm[256]]()
        val jwe = Jwe[AesGcm[256]](sonnet, key).in[Text].as[Jwe]
        (jwe.header.algorithm, jwe.encryptedKey.length, jwe.decrypt[AesGcm[256]](key).read[Text])
      . assert(_ == (t"dir", 0, sonnet))

      test(m"Direct encryption with an AES-CBC-HMAC key round-trips"):
        val key = SymmetricKey.generate[AesCbcHmac[256]]()
        Jwe[AesCbcHmac[256]](sonnet, key).in[Text].as[Jwe].decrypt[AesCbcHmac[256]](key).read[Text]
      . assert(_ == sonnet)

      test(m"A direct key must be a key of the content cipher"):
        val key = SymmetricKey.generate[AesGcm[128]]()
        capture[Jose.Error](Jwe[AesGcm[256]](sonnet, key)).reason
      . assert(_ == Jose.Error.Reason.Mismatch(t"A128GCM", t"A256GCM"))

      test(m"A192GCMKW is chosen in the header and round-trips"):
        val key = SymmetricKey.generate[AesGcm[192]]()
        val jwe = Jwe[AesGcm[256]](sonnet, key, Jose.Header(algorithm = t"A192GCMKW")).in[Text].as[Jwe]
        (jwe.header.vector.present, jwe.header.tag.present, jwe.decrypt[AesGcm[256]](key).read[Text])
      . assert(_ == (true, true, sonnet))

      test(m"ECDH-ES over P-256 derives the content key directly"):
        val key = PrivateKey.generate[Ecdh[256]]()
        val jwe = Jwe[AesGcm[128]](sonnet, key.public).in[Text].as[Jwe]
        (jwe.header.algorithm, jwe.encryptedKey.length, jwe.decrypt[AesGcm[128]](key).read[Text])
      . assert(_ == (t"ECDH-ES", 0, sonnet))

      test(m"ECDH-ES sends its ephemeral key on the recipient's curve"):
        val key = PrivateKey.generate[Ecdh[384]]()
        Jwe[AesGcm[256]](sonnet, key.public).header.ephemeralKey.let(_.curve)
      . assert(_ == t"P-384")

      test(m"ECDH-ES+A128KW over P-521 round-trips"):
        val key = PrivateKey.generate[Ecdh[521]]()
        val header = Jose.Header(algorithm = t"ECDH-ES+A128KW")
        val jwe = Jwe[AesCbcHmac[256]](sonnet, key.public, header).in[Text].as[Jwe]
        (jwe.header.algorithm, jwe.decrypt[AesCbcHmac[256]](key).read[Text])
      . assert(_ == (t"ECDH-ES+A128KW", sonnet))

      test(m"ECDH-ES+A256KW with party information round-trips"):
        val key = PrivateKey.generate[Ecdh[256]]()
        val header = Jose.Header(algorithm = t"ECDH-ES+A256KW", producer = t"QWxpY2U", recipient = t"Qm9i")
        Jwe[AesGcm[256]](sonnet, key.public, header).in[Text].as[Jwe].decrypt[AesGcm[256]](key).read[Text]
      . assert(_ == sonnet)

      test(m"ECDH-ES over X25519 round-trips"):
        val key = PrivateKey.generate[X25519]()
        val jwe = Jwe[AesGcm[256]](sonnet, key.public).in[Text].as[Jwe]
        (jwe.header.ephemeralKey.let(_.curve), jwe.decrypt[AesGcm[256]](key).read[Text])
      . assert(_ == (t"X25519", sonnet))

      test(m"ECDH-ES does not decrypt under another key"):
        val key = PrivateKey.generate[Ecdh[256]]()
        val other = PrivateKey.generate[Ecdh[256]]()
        val jwe = Jwe[AesGcm[128]](sonnet, key.public).in[Text].as[Jwe]
        capture[Jose.Error](jwe.decrypt[AesGcm[128]](other)).reason
      . assert(_ == Jose.Error.Reason.Undecryptable)

      test(m"An algorithm outside the key's family is refused"):
        val key = PrivateKey.generate[Ecdh[256]]()
        capture[Jose.Error](Jwe[AesGcm[128]](sonnet, key.public, Jose.Header(algorithm = t"RSA-OAEP"))).reason
      . assert(_ == Jose.Error.Reason.Mismatch(t"ECDH-ES", t"RSA-OAEP"))

      test(m"PBES2-HS512+A256KW round-trips with the header's iteration count"):
        val password = Password(t"Thus with a kiss I die.")
        val header = Jose.Header(algorithm = t"PBES2-HS512+A256KW", iterations = 1000)
        val jwe = Jwe[AesGcm[256]](sonnet, password, header).in[Text].as[Jwe]
        (jwe.header.iterations, jwe.decrypt[AesGcm[256]](password).read[Text])
      . assert(_ == (1000, sonnet))

      test(m"An absurd PBES2 iteration count is refused before any work is done"):
        val password = Password(t"Thus with a kiss I die.")
        val header = Jose.Header(algorithm = t"PBES2-HS256+A128KW", iterations = 1000)
        val jwe = Jwe[AesGcm[128]](sonnet, password, header)
        val costly = Jose.Header.read(jwe.protection.or(t"")).copy(iterations = 2000000000)
        capture[Jose.Error](jwe.copy(protection = costly.encoded).decrypt[AesGcm[128]](password)).reason
      . assert(_ == Jose.Error.Reason.Unsupported(t"PBES2 with 2000000000 iterations"))

      test(m"The flattened serialization round-trips"):
        val key = SymmetricKey.generate[AesKeyWrap[128]]()
        val json = Jwe[AesGcm[128]](sonnet, key).flattened
        Jwe.read(json).head.let(_.decrypt[AesGcm[128]](key).read[Text])
      . assert(_ == sonnet)

    suite(m"JWK"):
      test(m"RFC 7517 A.1: a key set is read, and a key found by its identifier"):
        keys(Vectors.publicKeys)(t"2011-04-29").let(_.keyType)
      . assert(_ == t"RSA")

      test(m"RFC 7517 A.1: a key that is not in the set is absent"):
        keys(Vectors.publicKeys)(t"1599")
      . assert(_ == Unset)

      test(m"RFC 7517 A.2: private keys in a set sign"):
        val key = keys(Vectors.privateKeys)(t"2011-04-29").let(_.privateKey[Rsa[2048]])
        key.let(key => Jws(sonnet, key).verify(key.public).read[Text])
      . assert(_ == sonnet)

      test(m"RFC 7517 A.3: a symmetric key in a set wraps"):
        val key = keys(Vectors.symmetricKeys).keys.head.let(_.symmetricKey[AesKeyWrap[128]])
        key.let(key => Jwe[AesGcm[128]](sonnet, key).decrypt[AesGcm[128]](key).read[Text])
      . assert(_ == sonnet)

      test(m"RFC 7638 3.1: the thumbprint of an RSA key"):
        jwk(Vectors.thumbprintKey).thumbprint[Sha2[256]]
      . assert(_ == t"NzbLsXh8uDCcd-6MNwXF4W_7noWXFZAfHkxZsRGC9Xs")

      test(m"RFC 8037 A.3: the thumbprint of an Ed25519 key"):
        jwk(Vectors.ed25519Key).thumbprint[Sha2[256]]
      . assert(_ == t"kPrK_qmxVWaYVA9wwBF6Iuo3vVzz7TxHCTwXBygrS4k")

      test(m"A thumbprint does not depend on a key's private members"):
        jwk(Vectors.ed25519Key).public.thumbprint[Sha2[256]]
      . assert(_ == t"kPrK_qmxVWaYVA9wwBF6Iuo3vVzz7TxHCTwXBygrS4k")

      test(m"An RSA public key is written as the JWK it was read from"):
        val key = jwk(Vectors.rs256Key)
        val written = Jwk(key.publicKey[Rsa[2048]])
        (written.modulus, written.exponent)
      . assert(_ == (jwk(Vectors.rs256Key).modulus, t"AQAB"))

      test(m"An RSA private key is written as the JWK it was read from"):
        val key = jwk(Vectors.rs256Key)
        Jwk(key.privateKey[Rsa[2048]], Divulgence)
      . assert(_ == jwk(Vectors.rs256Key))

      test(m"An EC private key is written as the JWK it was read from"):
        val key = jwk(Vectors.es256Key)
        Jwk(key.privateKey[Ecdsa[256]], Divulgence)
      . assert(_ == jwk(Vectors.es256Key))

      test(m"A P-521 key is written as the JWK it was read from"):
        val key = jwk(Vectors.es512Key)
        Jwk(key.privateKey[Ecdsa[521]], Divulgence)
      . assert(_ == jwk(Vectors.es512Key))

      test(m"An Ed25519 private key is written as the JWK it was read from"):
        val key = jwk(Vectors.ed25519Key)
        Jwk(key.privateKey[Ed25519], Divulgence)
      . assert(_ == jwk(Vectors.ed25519Key))

      test(m"A symmetric key is written as an oct JWK"):
        val key = jwk(Vectors.aesKwKey).symmetricKey[AesKeyWrap[128]]
        Jwk(key, Divulgence)
      . assert(_ == jwk(Vectors.aesKwKey))

      test(m"A public JWK has no private members"):
        jwk(Vectors.rs256Key).public.d
      . assert(_ == Unset)

      test(m"A public JWK cannot be read as a private key"):
        capture[Jose.Error](jwk(Vectors.es256Key).public.privateKey[Ecdsa[256]]).reason
      . assert(_ == Jose.Error.Reason.BadKey(t"the key has no d member"))

      test(m"A key of one type is not read as another"):
        capture[Jose.Error](jwk(Vectors.es256Key).publicKey[Rsa[2048]]).reason
      . assert(_ == Jose.Error.Reason.BadKey(t"a key of type RSA was expected but it is of type EC"))

      test(m"A key on one curve is not read as a key on another"):
        capture[Jose.Error](jwk(Vectors.es256Key).publicKey[Ecdsa[384]]).reason
      . assert(_ == Jose.Error.Reason.BadKey(t"the key is not on the curve P-384"))

      test(m"A generated key round-trips through its JWK text"):
        val key = PrivateKey.generate[Ecdsa[384]]()
        val text = Jwk(key.public).in[Json].show
        Jws(sonnet, key).verify(jwk(text).publicKey[Ecdsa[384]]).read[Text]
      . assert(_ == sonnet)

    suite(m"JWT"):
      // One second before the expiry of the token in RFC 7519 §3.1.
      val early: Clock = Clock.fixed(Instant.of[Unix](1300819379000L))
      val late: Clock = Clock.fixed(Instant.of[Unix](1300819380000L))

      test(m"RFC 7519 3.1: the example token verifies before it expires"):
        given Clock = early
        val key = jwk(Vectors.hs256Key).symmetricKey[HmacCipher[Sha2[256]]]
        Jwt.verify(Vectors.hs256Jws, key).claims.issuer
      . assert(_ == t"joe")

      test(m"RFC 7519 3.1: the example token has expired at its expiry time"):
        given Clock = late
        val key = jwk(Vectors.hs256Key).symmetricKey[HmacCipher[Sha2[256]]]
        capture[Jose.Error](Jwt.verify(Vectors.hs256Jws, key)).reason
      . assert(_ == Jose.Error.Reason.Expired)

      test(m"Leeway admits a token just past its expiry"):
        given Clock = late
        val key = jwk(Vectors.hs256Key).symmetricKey[HmacCipher[Sha2[256]]]
        Jwt.read(Vectors.hs256Jws.as[Jws].verify(key)).validate(leeway = 30).claims.expiry.let(_.long)
      . assert(_ == 1300819380000L)

      test(m"An application's own claim is read from the token"):
        val key = jwk(Vectors.hs256Key).symmetricKey[HmacCipher[Sha2[256]]]
        Jwt.read(Vectors.hs256Jws.as[Jws].verify(key)).json(t"http://example.com/is_root").as[Boolean]
      . assert(identity)

      test(m"A token of registered and custom claims round-trips"):
        given Clock = early
        val key = PrivateKey.generate[Ecdsa[256]]()

        val claims =
          Jwt.Claims
            ( issuer   = t"globe",
              subject  = t"burbage",
              audience = List(t"groundlings"),
              expiry   = Instant.of[Unix](1300819380000L),
              id       = t"1599" )

        val token = Jwt(claims, Role(t"Hamlet", t"Hamlet")).sign(key).in[Text]
        val jwt = Jwt.verify(token, key.public)
        (jwt.claims, jwt.json.as[Role])
      . assert(_ == (
          Jwt.Claims
            ( issuer   = t"globe",
              subject  = t"burbage",
              audience = List(t"groundlings"),
              expiry   = Instant.of[Unix](1300819380000L),
              id       = t"1599" ),
          Role(t"Hamlet", t"Hamlet") ))

      test(m"A signed token's header says it is a JWT"):
        val key = PrivateKey.generate[Ed25519]()
        Jwt(Jwt.Claims(issuer = t"globe")).sign(key).header.mediaType
      . assert(_ == t"JWT")

      test(m"A single audience is written as a string and several as an array"):
        val one = Jwt(Jwt.Claims(audience = List(t"groundlings"))).json.show
        val two = Jwt(Jwt.Claims(audience = List(t"groundlings", t"gallery"))).json.show
        (one, two)
      . assert(_ == (t"""{"aud":"groundlings"}""", t"""{"aud":["groundlings","gallery"]}"""))

      test(m"A token for several audiences is read"):
        Jwt.read(t"""{"aud":["groundlings","gallery"]}""".in[Data]).claims.audience
      . assert(_ == List(t"groundlings", t"gallery"))

      test(m"A token is refused before its time"):
        given Clock = early
        val claims = Jwt.Claims(notBefore = Instant.of[Unix](1300819380000L))
        capture[Jose.Error](Jwt(claims).validate()).reason
      . assert(_ == Jose.Error.Reason.Premature)

      test(m"A token from an unexpected issuer is refused"):
        given Clock = early
        capture[Jose.Error](Jwt(Jwt.Claims(issuer = t"rose")).validate(issuer = t"globe")).reason
      . assert(_ == Jose.Error.Reason.Issuer(t"rose"))

      test(m"A token for another audience is refused"):
        given Clock = early
        val jwt = Jwt(Jwt.Claims(audience = List(t"gallery")))
        capture[Jose.Error](jwt.validate(audience = t"groundlings")).reason
      . assert(_ == Jose.Error.Reason.Audience)

      test(m"A token with no audience fails an audience check"):
        given Clock = early
        capture[Jose.Error](Jwt(Jwt.Claims()).validate(audience = t"groundlings")).reason
      . assert(_ == Jose.Error.Reason.Audience)

      test(m"A claim of the wrong type is an error"):
        capture[Jose.Error](Jwt.read(t"""{"exp":"tomorrow"}""".in[Data])).reason
      . assert(_ == Jose.Error.Reason.Claim(t"exp"))

      test(m"A token with a fractional expiry is read to the millisecond"):
        Jwt.read(t"""{"exp":1300819380.5}""".in[Data]).claims.expiry.let(_.long)
      . assert(_ == 1300819380500L)
