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

import java.security as js, js.spec as jss, js.interfaces as jsi
import javax.crypto as jc, javax.crypto.spec.*

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

// The default `Crypto` provider, backed by the JDK's standard crypto (JCE /
// `java.security`). This is the single home of all `javax.crypto.*` and
// `java.security.*` usage in enigmatic; every other module reaches these
// algorithms only through the `Crypto` contract.
object JavaStdlibCrypto extends Crypto:
  def random: Crypto.Random = new Crypto.Random:
    def bytes(size: Int): Data =
      val output = new Array[Byte](size)
      js.SecureRandom().nextBytes(output)
      output.immutable(using Unsafe)

  def aes:       Crypto.SymmetricCipher = symmetric(t"AES")
  def des:       Crypto.SymmetricCipher = symmetric(t"DES")
  def tripleDes: Crypto.SymmetricCipher = symmetric(t"DESede")
  def blowfish:  Crypto.SymmetricCipher = symmetric(t"Blowfish")
  def rc2:       Crypto.SymmetricCipher = symmetric(t"RC2")

  def hmac(algorithm: Text): Crypto.Mac = new Crypto.Mac:
    def mac(key: Data, data: Data): Data =
      val mac = jc.Mac.getInstance(algorithm.s).nn
      mac.init(SecretKeySpec(key.to(Array), algorithm.s))
      mac.doFinal(data.to(Array)).nn.immutable(using Unsafe)

  def rsa: Crypto.PublicKeyCipher = new Crypto.PublicKeyCipher:
    private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("RSA").nn
    private def cipher(): jc.Cipher = jc.Cipher.getInstance("RSA").nn

    def encrypt(input: Data, publicKey: Data): Data =
      val instance = cipher()
      val key = keyFactory().generatePublic(jss.X509EncodedKeySpec(publicKey.mutable(using Unsafe)))
      instance.init(jc.Cipher.ENCRYPT_MODE, key)
      instance.doFinal(input.mutable(using Unsafe)).nn.immutable(using Unsafe)

    def decrypt(input: Data, privateKey: Data): Data =
      val instance = cipher()
      val key = keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(privateKey.mutable(using Unsafe)))
      instance.init(jc.Cipher.DECRYPT_MODE, key)
      instance.doFinal(input.mutable(using Unsafe)).nn.immutable(using Unsafe)

    def generateKeyPair(bits: Int): Data =
      val generator = js.KeyPairGenerator.getInstance("RSA").nn
      generator.initialize(bits)
      generator.generateKeyPair().nn.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

    def privateToPublic(privateKey: Data): Data =
      val javaKey =
        keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(privateKey.mutable(using Unsafe))).nn

      val key = javaKey match
        case key: jsi.RSAPrivateCrtKey => key
        case key: js.PrivateKey        => panic(m"unexpected private key type")

      val spec = jss.RSAPublicKeySpec(key.getModulus, key.getPublicExponent)
      keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  def dsa: Crypto.SignatureScheme = new Crypto.SignatureScheme:
    private def signature(): js.Signature = js.Signature.getInstance("DSA").nn
    private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("DSA").nn

    def sign(data: Data, privateKey: Data): Data =
      val sig = signature()
      sig.initSign(keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(privateKey.to(Array))))
      sig.update(data.to(Array))
      sig.sign().nn.immutable(using Unsafe)

    def verify(data: Data, signature0: Data, publicKey: Data): Boolean =
      val sig = signature()
      sig.initVerify(keyFactory().generatePublic(jss.X509EncodedKeySpec(publicKey.to(Array))))
      sig.update(data.to(Array))
      sig.verify(signature0.to(Array))

    def generateKeyPair(bits: Int): Data =
      val generator = js.KeyPairGenerator.getInstance("DSA").nn
      generator.initialize(bits, js.SecureRandom())
      generator.generateKeyPair().nn.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

    def privateToPublic(privateKey: Data): Data =
      val key = keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(privateKey.to(Array))).nn match
        case key: jsi.DSAPrivateKey => key
        case key: js.PrivateKey     => panic(m"unexpected private key type")

      val params = key.getParams.nn
      val y = params.getG.nn.modPow(key.getX, params.getP)
      val spec = jss.DSAPublicKeySpec(y, params.getP, params.getQ, params.getG)
      keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  // Shared implementation for all JCE block ciphers; `algorithm` is the bare key
  // algorithm (e.g. `t"AES"`), used for `SecretKeySpec` and `KeyGenerator`, while
  // the full `transformation` (e.g. `t"AES/CBC/PKCS5Padding"`) drives the cipher.
  private def symmetric(algorithm: Text): Crypto.SymmetricCipher = new Crypto.SymmetricCipher:
    private def makeKey(key: Data): SecretKeySpec = SecretKeySpec(key.mutable(using Unsafe), algorithm.s)

    def blockSize(transformation: Text): Int = jc.Cipher.getInstance(transformation.s).nn.getBlockSize

    def generateKey(bits: Int): Data =
      val keyGen = jc.KeyGenerator.getInstance(algorithm.s).nn
      keyGen.init(bits)
      keyGen.generateKey().nn.getEncoded.nn.immutable(using Unsafe)

    def encrypt(transformation: Text, key: Data, iv: Optional[Data], data: Data): Data =
      val cipher = jc.Cipher.getInstance(transformation.s).nn

      iv.lay:
        cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key))
        cipher.doFinal(data.mutable(using Unsafe)).nn.immutable(using Unsafe)
      .apply: iv =>
        val ivBytes = iv.mutable(using Unsafe)
        cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key), IvParameterSpec(ivBytes))
        (ivBytes ++ cipher.doFinal(data.mutable(using Unsafe)).nn).immutable(using Unsafe)

    def decrypt(transformation: Text, key: Data, ivSize: Optional[Int], data: Data): Data =
      val cipher = jc.Cipher.getInstance(transformation.s).nn
      val input = data.mutable(using Unsafe)

      ivSize.lay:
        cipher.init(jc.Cipher.DECRYPT_MODE, makeKey(key))
        cipher.doFinal(input).nn.immutable(using Unsafe)
      .apply: size =>
        cipher.init(jc.Cipher.DECRYPT_MODE, makeKey(key), IvParameterSpec(input.take(size)))
        cipher.doFinal(input.drop(size)).nn.immutable(using Unsafe)

    def stream(transformation: Text, key: Data, iv: Optional[Data]): CipherSession =
      val cipher = jc.Cipher.getInstance(transformation.s).nn

      iv.lay(cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key))): iv =>
        cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key), IvParameterSpec(iv.mutable(using Unsafe)))

      new CipherSession:
        def update(chunk: Data): Data =
          cipher.update(chunk.mutable(using Unsafe)).nn.immutable(using Unsafe)

        def finish(): Data = cipher.doFinal().nn.immutable(using Unsafe)
