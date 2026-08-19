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

import java.security as js, js.spec as jss, js.interfaces as jsi
import javax.crypto as jc, javax.crypto.spec.*
import proscenium.compat.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*
import denominative.asymptotics.linearSizeComplexity

// The default `Crypto` provider, backed by the JDK's standard crypto (JCE /
// `java.security`). This is the single home of all `javax.crypto.*` and
// `java.security.*` usage in enigmatic; every other module reaches these
// algorithms only through the `Crypto` contract.
object JavaStdlibCrypto extends Crypto:
  def random: Crypto.Random = new Crypto.Random:
    def bytes(size: Int): Data =
      val output = Array[Byte](size)
      js.SecureRandom().nextBytes(output.raw)
      Array.freeze(output)

  def aes:       Crypto.SymmetricCipher = symmetric(t"AES")
  def des:       Crypto.SymmetricCipher = symmetric(t"DES")
  def tripleDes: Crypto.SymmetricCipher = symmetric(t"DESede")
  def blowfish:  Crypto.SymmetricCipher = symmetric(t"Blowfish")
  def rc2:       Crypto.SymmetricCipher = symmetric(t"RC2")

  def hmac(algorithm: Text): Crypto.Mac = new Crypto.Mac:
    def mac(key: Data, data: Data): Data =
      val mac = jc.Mac.getInstance(algorithm.s).nn
      mac.init(SecretKeySpec(key.readable.to(scala.Array), algorithm.s))
      Array.unsafeFrozen(mac.doFinal(data.readable.to(scala.Array)).nn)

  def rsa: Crypto.PublicKeyCipher = new Crypto.PublicKeyCipher:
    private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("RSA").nn
    private def cipher(): jc.Cipher = jc.Cipher.getInstance("RSA").nn

    def encrypt(input: Data, publicKey: Data): Data =
      val instance = cipher()
      // Both key specs and `Cipher.doFinal` copy the array they are handed; `doFinal` and
      // `getEncoded` return a fresh one.
      val key = keyFactory().generatePublic(jss.X509EncodedKeySpec(Array.unsafeJvm(publicKey)))
      instance.init(jc.Cipher.ENCRYPT_MODE, key)
      Array.unsafeFrozen(instance.doFinal(Array.unsafeJvm(input)).nn)

    def decrypt(input: Data, privateKey: Data): Data =
      val instance = cipher()

      val key =
        keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(Array.unsafeJvm(privateKey)))

      instance.init(jc.Cipher.DECRYPT_MODE, key)
      Array.unsafeFrozen(instance.doFinal(Array.unsafeJvm(input)).nn)

    def generateKeyPair(bits: Int): Data =
      val generator = js.KeyPairGenerator.getInstance("RSA").nn
      generator.initialize(bits)
      Array.unsafeFrozen(generator.generateKeyPair().nn.getPrivate.nn.getEncoded.nn)

    def privateToPublic(privateKey: Data): Data =
      val javaKey =
        keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(Array.unsafeJvm(privateKey))).nn

      val key = javaKey match
        case key: jsi.RSAPrivateCrtKey => key
        case key: js.PrivateKey        => panic(m"unexpected private key type")

      val spec = jss.RSAPublicKeySpec(key.getModulus, key.getPublicExponent)
      Array.unsafeFrozen(keyFactory().generatePublic(spec).nn.getEncoded.nn)

  def rsaSignature(digest: Text): Crypto.SignatureScheme =
    new Signatory(t"${digest}withRSA", t"RSA"):
      def generateKeyPair(bits: Int): Data =
        val generator = js.KeyPairGenerator.getInstance("RSA").nn
        generator.initialize(bits)

        generator.generateKeyPair().nn.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

      def privateToPublic(privateKey: Data): Data = JavaStdlibCrypto.rsa.privateToPublic(privateKey)

  // ECDSA over a NIST prime curve. The curve is chosen by key size, since `KeyPairGenerator` needs
  // a curve name rather than a bit count for EC; P-521 really is 521 bits, not 512.
  def ecdsa(digest: Text): Crypto.SignatureScheme =
    new Signatory(t"${digest}withECDSA", t"EC"):
      def generateKeyPair(bits: Int): Data =
        val curve = bits match
          case 256 => "secp256r1"
          case 384 => "secp384r1"
          case 521 => "secp521r1"
          case _   => panic(m"there is no NIST prime curve of $bits bits")

        val generator = js.KeyPairGenerator.getInstance("EC").nn
        generator.initialize(jss.ECGenParameterSpec(curve), js.SecureRandom())
        val pair = generator.generateKeyPair().nn

        val privateKey = pair.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)
        val publicKey = pair.getPublic.nn.getEncoded.nn.immutable(using Unsafe)

        embedPublicKey(privateKey, publicKey)

      def privateToPublic(privateKey: Data): Data = extractPublicKey(privateKey)

  // An EC public key is `d·G`, and recovering it from the scalar `d` needs curve arithmetic that
  // neither `KeyFactory` nor `BigInteger` will do — and that this module has no business
  // reimplementing. RFC 5915 provides for the public key to be carried alongside the scalar, in an
  // optional `[1] EXPLICIT BIT STRING`, which is what OpenSSL emits and what the JDK accepts but
  // does not itself write. Embedding it at generation makes `privateToPublic` a lookup.
  //
  // `PrivateKeyInfo` is `SEQUENCE { version, algorithm, OCTET STRING }`, whose octets hold RFC
  // 5915's `ECPrivateKey`; `SubjectPublicKeyInfo` is `SEQUENCE { algorithm, BIT STRING }` with the
  // same `algorithm` field, so no part of it has to be rebuilt.
  private def embedPublicKey(privateKey: Data, publicKey: Data): Data =
    val bits = decode(publicKey) match
      case Asn1.Sequence(List(_, bits: Asn1.BitString)) => bits
      case _                                            => panic(m"the EC public key was bad")

    decode(privateKey) match
      case Asn1.Sequence(List(version, algorithm, Asn1.OctetString(inner))) =>
        val extended: Asn1 = decode(inner) match
          case Asn1.Sequence(elements) => Asn1.Sequence(elements :+ Asn1.Tagged(1, true, bits))
          case _                       => panic(m"the EC private key was not an ECPrivateKey")

        val octets = Asn1.OctetString(extended.in[Der].data)
        val info: Asn1 = Asn1.Sequence(List(version, algorithm, octets))

        info.in[Der].data

      case _ => panic(m"the EC private key was not a PrivateKeyInfo")

  private def extractPublicKey(privateKey: Data): Data =
    decode(privateKey) match
      case Asn1.Sequence(List(_, algorithm, Asn1.OctetString(inner))) =>
        val bits = decode(inner) match
          case Asn1.Sequence(elements) => elements.glean:
            case Asn1.Tagged(1, true, bits: Asn1.BitString) => bits

          case _ => Unset

        bits.lay(panic(m"the EC private key carried no public key")): bits =>
          val info: Asn1 = Asn1.Sequence(List(algorithm, bits))
          info.in[Der].data

      case _ => panic(m"the EC private key was not a PrivateKeyInfo")

  // ML-DSA (FIPS 204), provided by the JDK from 24 (JEP 497). The parameterized algorithm names
  // pin the parameter set, so a key generated at one strength cannot be used at another. ML-DSA
  // signs the message directly, so no digest participates in the transformation name.
  def mlDsa(level: Int): Crypto.SignatureScheme = new Crypto.SignatureScheme:
    private val name: String = level match
      case 44 => "ML-DSA-44"
      case 65 => "ML-DSA-65"
      case 87 => "ML-DSA-87"
      case _  => panic(m"there is no ML-DSA parameter set of strength $level")

    private def instance(): js.Signature = mlDsaAvailable(js.Signature.getInstance(name).nn)
    private def keyFactory(): js.KeyFactory = mlDsaAvailable(js.KeyFactory.getInstance(name).nn)

    def sign(data: Data, privateKey: Data): Data =
      val sig = instance()
      val spec = jss.PKCS8EncodedKeySpec(Array.unsafeJvm(mlDsaStrip(privateKey)))
      sig.initSign(keyFactory().generatePrivate(spec))
      sig.update(Array.unsafeJvm(data))

      sig.sign().nn.immutable(using Unsafe)

    def verify(data: Data, signature0: Data, publicKey: Data): Boolean =
      val sig = instance()
      sig.initVerify(keyFactory().generatePublic(jss.X509EncodedKeySpec(Array.unsafeJvm(publicKey))))
      sig.update(Array.unsafeJvm(data))

      sig.verify(Array.unsafeJvm(signature0))

    def generateKeyPair(bits: Int): Data =
      val generator = mlDsaAvailable(js.KeyPairGenerator.getInstance(name).nn)
      val pair = generator.generateKeyPair().nn

      val privateKey = pair.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)
      val publicKey = pair.getPublic.nn.getEncoded.nn.immutable(using Unsafe)

      mlDsaEmbed(privateKey, publicKey)

    def privateToPublic(privateKey: Data): Data = mlDsaExtract(privateKey)

  // The JDK grew ML-DSA at 24; on earlier releases `getInstance` is the single point of failure,
  // so it is the one place the version requirement is reported.
  private def mlDsaAvailable[value](operation: => value): value =
    try operation catch case error: js.NoSuchAlgorithmException =>
      panic(m"the JDK does not provide ML-DSA; JDK 24 or later is required")

  // An ML-DSA private key offers no JCA route back to its public key, so — as with EC — the
  // public key is embedded at generation. RFC 5958's `OneAsymmetricKey` reserves `[1]` at the
  // top level of the `PrivateKeyInfo` sequence for exactly this, though it is written here in
  // explicit form (the ASN.1 layer cannot round-trip implicit tags without a schema) and the
  // version integer is left untouched. The embedded element never reaches the JDK: `mlDsaStrip`
  // reduces the key to its canonical three-element `PrivateKeyInfo` before any JCA call.
  private def mlDsaEmbed(privateKey: Data, publicKey: Data): Data =
    val bits = decode(publicKey) match
      case Asn1.Sequence(List(_, bits: Asn1.BitString)) => bits
      case _                                            => panic(m"the ML-DSA public key was bad")

    decode(privateKey) match
      case Asn1.Sequence(List(version, algorithm, octets: Asn1.OctetString)) =>
        val info: Asn1 = Asn1.Sequence(List(version, algorithm, octets, Asn1.Tagged(1, true, bits)))
        info.in[Der].data

      case _ =>
        panic(m"the ML-DSA private key was not a PrivateKeyInfo")

  private def mlDsaStrip(privateKey: Data): Data =
    decode(privateKey) match
      case Asn1.Sequence(version :: algorithm :: (octets: Asn1.OctetString) :: _) =>
        val info: Asn1 = Asn1.Sequence(List(version, algorithm, octets))
        info.in[Der].data

      case _ =>
        panic(m"the ML-DSA private key was not a PrivateKeyInfo")

  private def mlDsaExtract(privateKey: Data): Data =
    decode(privateKey) match
      case Asn1.Sequence(_ :: algorithm :: _ :: rest) =>
        val bits = rest.glean:
          case Asn1.Tagged(1, _, bits: Asn1.BitString) => bits

        bits.lay(panic(m"the ML-DSA private key carried no public key")): bits =>
          val info: Asn1 = Asn1.Sequence(List(algorithm, bits))
          info.in[Der].data

      case _ =>
        panic(m"the ML-DSA private key was not a PrivateKeyInfo")

  // Key material this module generated itself; a failure to parse it back is a defect, not an
  // input error.
  private def decode(data: Data): Asn1 = unsafely(Der(data).as[Asn1])

  // The shared shape of the two digest-parameterized signature schemes: a JCE `Signature`
  // transformation and a `KeyFactory` for the key algorithm. Key generation and public-key
  // recovery differ per algorithm and are left abstract.
  private abstract class Signatory(transformation: Text, algorithm: Text)
  extends Crypto.SignatureScheme:
    private def instance(): js.Signature = js.Signature.getInstance(transformation.s).nn
    private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance(algorithm.s).nn

    def sign(data: Data, privateKey: Data): Data =
      val sig = instance()
      sig.initSign(keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(Array.unsafeJvm(privateKey))))
      sig.update(Array.unsafeJvm(data))

      sig.sign().nn.immutable(using Unsafe)

    def verify(data: Data, signature0: Data, publicKey: Data): Boolean =
      val sig = instance()
      sig.initVerify(keyFactory().generatePublic(jss.X509EncodedKeySpec(Array.unsafeJvm(publicKey))))
      sig.update(Array.unsafeJvm(data))

      sig.verify(Array.unsafeJvm(signature0))

  def dsa: Crypto.SignatureScheme = new Crypto.SignatureScheme:
    private def signature(): js.Signature = js.Signature.getInstance("DSA").nn
    private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("DSA").nn

    def sign(data: Data, privateKey: Data): Data =
      val sig = signature()
      sig.initSign(keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(privateKey.readable.to(scala.Array))))
      sig.update(data.readable.to(scala.Array))
      Array.unsafeFrozen(sig.sign().nn)

    def verify(data: Data, signature0: Data, publicKey: Data): Boolean =
      val sig = signature()
      sig.initVerify(keyFactory().generatePublic(jss.X509EncodedKeySpec(publicKey.readable.to(scala.Array))))
      sig.update(data.readable.to(scala.Array))
      sig.verify(signature0.readable.to(scala.Array))

    def generateKeyPair(bits: Int): Data =
      val generator = js.KeyPairGenerator.getInstance("DSA").nn
      generator.initialize(bits, js.SecureRandom())
      Array.unsafeFrozen(generator.generateKeyPair().nn.getPrivate.nn.getEncoded.nn)

    def privateToPublic(privateKey: Data): Data =
      val key = keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(privateKey.readable.to(scala.Array))).nn match
        case key: jsi.DSAPrivateKey => key
        case key: js.PrivateKey     => panic(m"unexpected private key type")

      val params = key.getParams.nn
      val y = params.getG.nn.modPow(key.getX, params.getP)
      val spec = jss.DSAPublicKeySpec(y, params.getP, params.getQ, params.getG)
      Array.unsafeFrozen(keyFactory().generatePublic(spec).nn.getEncoded.nn)

  // Shared implementation for all JCE block ciphers; `algorithm` is the bare key
  // algorithm (e.g. `t"AES"`), used for `SecretKeySpec` and `KeyGenerator`, while
  // the full `transformation` (e.g. `t"AES/CBC/PKCS5Padding"`) drives the cipher.
  private def symmetric(algorithm: Text): Crypto.SymmetricCipher = new Crypto.SymmetricCipher:
    // `SecretKeySpec` copies the key material it is given.
    private def makeKey(key: Data): SecretKeySpec =
      SecretKeySpec(Array.unsafeJvm(key), algorithm.s)

    def blockSize(transformation: Text): Int =
      jc.Cipher.getInstance(transformation.s).nn.getBlockSize

    def generateKey(bits: Int): Data =
      val keyGen = jc.KeyGenerator.getInstance(algorithm.s).nn
      keyGen.init(bits)
      Array.unsafeFrozen(keyGen.generateKey().nn.getEncoded.nn)

    def encrypt(transformation: Text, key: Data, iv: Optional[Data], data: Data): Data =
      val cipher = jc.Cipher.getInstance(transformation.s).nn

      iv.lay:
        cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key))
        Array.unsafeFrozen(cipher.doFinal(Array.unsafeJvm(data)).nn)

      .apply: iv =>
        // `IvParameterSpec` copies the IV, and `++` builds a fresh array from both operands.
        val ivBytes = Array.unsafeJvm(iv)
        cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key), IvParameterSpec(ivBytes))
        Array.unsafeFrozen(ivBytes ++ cipher.doFinal(Array.unsafeJvm(data)).nn)

    def decrypt(transformation: Text, key: Data, ivSize: Optional[Int], data: Data): Data =
      val cipher = jc.Cipher.getInstance(transformation.s).nn
      // `doFinal` reads its input, and `take`/`drop` copy, so one view serves the whole method.
      val input = Array.unsafeJvm(data)

      ivSize.lay:
        cipher.init(jc.Cipher.DECRYPT_MODE, makeKey(key))
        Array.unsafeFrozen(cipher.doFinal(input).nn)

      .apply: size =>
        cipher.init(jc.Cipher.DECRYPT_MODE, makeKey(key), IvParameterSpec(input.take(size)))
        Array.unsafeFrozen(cipher.doFinal(input.drop(size)).nn)

    def stream(transformation: Text, key: Data, iv: Optional[Data]): Cipher.Session =
      session(transformation, key, iv, jc.Cipher.ENCRYPT_MODE)

    def decryptStream(transformation: Text, key: Data, iv: Optional[Data]): Cipher.Session =
      session(transformation, key, iv, jc.Cipher.DECRYPT_MODE)

    private def session(transformation: Text, key: Data, iv: Optional[Data], opmode: Int)
    :   Cipher.Session =

      val cipher = jc.Cipher.getInstance(transformation.s).nn

      iv.lay(cipher.init(opmode, makeKey(key))): iv =>
        cipher.init(opmode, makeKey(key), IvParameterSpec(Array.unsafeJvm(iv)))

      new Cipher.Session:
        def update(chunk: Data): Data =
          // `Cipher.update` returns null when a block cipher has buffered the
          // whole input pending a complete block, and a fresh array otherwise.
          cipher.update(Array.unsafeJvm(chunk)) match
            case null                   => Data()
            case out: scala.Array[Byte] => Array.unsafeFrozen(out)

        def finish(): Data = Array.unsafeFrozen(cipher.doFinal().nn)
