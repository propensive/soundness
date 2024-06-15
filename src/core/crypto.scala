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

import javax.crypto as jc, javax.crypto.spec.*
import java.security as js, js.spec.*

import rudiments.*
import vacuous.*
import fulminate.*
import gossamer.*
import contingency.*
import spectacular.*
import anticipation.*

trait Cipher:
  type Size <: Nat
  def keySize: Size
  def privateToPublic(key: Bytes): Bytes
  def genKey(): Bytes

trait Encryption:
  def encrypt(value: Bytes, privateKey: Bytes): Bytes
  def decrypt(bytes: Bytes, publicKey: Bytes): Bytes

trait Signing:
  def sign(data: Bytes, privateKey: Bytes): Bytes
  def verify(data: Bytes, signature: Bytes, publicKey: Bytes): Boolean

trait Symmetric

object Signature:
  given [SignatureType <: Cipher](using Alphabet[Base64]) => Signature[SignatureType] is Showable = sig => t"Signature(${sig.bytes.serialize[Base64]})"
  given [CipherType <: Cipher] => Signature[CipherType] is Encodable in Bytes = _.bytes

case class Signature[+CipherType <: Cipher](bytes: Bytes)

object ExposeSecretKey

object PublicKey:
  given [KeyType <: Cipher](using Alphabet[Hex]) => PublicKey[KeyType] is Showable = key => t"PublicKey(${key.bytes.serialize[Hex]})"
  given [CipherType <: Cipher] => PublicKey[CipherType] is Encodable in Bytes = _.bytes

case class PublicKey[CipherType <: Cipher](bytes: Bytes):
  def encrypt[ValueType: Encodable in Bytes](value: ValueType)(using algorithm: CipherType & Encryption)
          : Bytes =

    algorithm.encrypt(value.binary, bytes)

  def verify[ValueType: Encodable in Bytes](value: ValueType, signature: Signature[CipherType])
      (using algorithm: CipherType & Signing)
          : Boolean =

    algorithm.verify(ValueType.encode(value), signature.bytes, bytes)

  def pem: Pem = Pem(PemLabel.PublicKey, bytes)

object PrivateKey:
  def generate[CipherType <: Cipher]()(using cipher: CipherType): PrivateKey[CipherType] =
    PrivateKey(cipher.genKey())

  // given [KeyType <: Cipher] => PrivateKey[KeyType] is Showable =
  //   key => t"PrivateKey(${key.privateBytes.digest[Sha2[256]].serialize[Base64]})"

case class PrivateKey[CipherType <: Cipher](private[gastronomy] val privateBytes: Bytes):
  def public(using cipher: CipherType): PublicKey[CipherType] =
    PublicKey(cipher.privateToPublic(privateBytes))

  def decrypt[ValueType: Decodable in Bytes](bytes: Bytes)(using cipher: CipherType & Encryption)
          : ValueType raises CryptoError =

    ValueType.decode(cipher.decrypt(bytes, privateBytes), false)

  def sign[ValueType: Encodable in Bytes](value: ValueType)(using cipher: CipherType & Signing)
          : Signature[CipherType] =

    Signature(cipher.sign(ValueType.encode(value), privateBytes))

  def pem(reveal: ExposeSecretKey.type): Pem = Pem(PemLabel.PrivateKey, privateBytes)

object SymmetricKey:
  given [CipherType <: Cipher] => SymmetricKey[CipherType] is Encodable in Bytes = _.bytes
  def generate[CipherType <: Cipher & Symmetric]()(using cipher: CipherType)
          : SymmetricKey[CipherType] =

    SymmetricKey(cipher.genKey())

class SymmetricKey[CipherType <: Cipher](private[gastronomy] val bytes: Bytes)
extends PrivateKey[CipherType](bytes):
  def encrypt[ValueType: Encodable in Bytes](value: ValueType)(using CipherType & Encryption): Bytes =
    public.encrypt(value)

  def verify[ValueType: Encodable in Bytes](value: ValueType, signature: Signature[CipherType])
      (using CipherType & Signing)
          : Boolean =

    public.verify(value, signature)

case class CryptoError(detail: Text) extends Error(msg"could not decode the encrypted data: $detail")

object Aes:
  given [BitsType <: 128 | 192 | 256: ValueOf] => Aes[BitsType] = Aes()

object Rsa:
  given [I <: 1024 | 2048: ValueOf] => Rsa[I] = Rsa()

object Dsa:
  given [BitsType <: 512 | 1024 | 2048 | 3072: ValueOf] => Dsa[BitsType] = Dsa()

class Aes[BitsType <: 128 | 192 | 256: ValueOf]() extends Cipher, Encryption, Symmetric:
  type Size = BitsType
  def keySize: BitsType = valueOf[BitsType]

  private def init() = jc.Cipher.getInstance("AES/ECB/PKCS5Padding")
  private def makeKey(key: Bytes): SecretKeySpec = SecretKeySpec(key.mutable(using Unsafe), "AES")

  def encrypt(bytes: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key))
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def decrypt(bytes: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    cipher.init(jc.Cipher.DECRYPT_MODE, makeKey(key))
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def genKey(): Bytes =
    val keyGen = jc.KeyGenerator.getInstance("AES").nn
    keyGen.init(keySize)

    keyGen.generateKey().nn.getEncoded.nn.immutable(using Unsafe)

  def privateToPublic(key: Bytes): Bytes = key

class Rsa[BitsType <: 1024 | 2048: ValueOf]() extends Cipher, Encryption:
  type Size = BitsType
  def keySize: BitsType = valueOf[BitsType]

  def privateToPublic(bytes: Bytes): Bytes =
    val privateKey = keyFactory().generatePrivate(PKCS8EncodedKeySpec(unsafely(bytes.mutable))).nn match
      case key: js.interfaces.RSAPrivateCrtKey => key
      case key: js.PrivateKey                  => throw Panic(msg"public key did not have the correct type")

    val spec = RSAPublicKeySpec(privateKey.getModulus, privateKey.getPublicExponent)
    keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  def decrypt(bytes: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    val privateKey = keyFactory().generatePrivate(PKCS8EncodedKeySpec(key.mutable(using Unsafe)))
    cipher.init(jc.Cipher.DECRYPT_MODE, privateKey)
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def encrypt(bytes: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    val publicKey = keyFactory().generatePublic(X509EncodedKeySpec(key.mutable(using Unsafe)))
    cipher.init(jc.Cipher.ENCRYPT_MODE, publicKey)
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def genKey(): Bytes =
    val generator = js.KeyPairGenerator.getInstance("RSA").nn
    generator.initialize(keySize)
    val keyPair = generator.generateKeyPair().nn
    keyPair.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

  private def init(): jc.Cipher = jc.Cipher.getInstance("RSA").nn
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("RSA").nn

class Dsa[BitsType <: 512 | 1024 | 2048 | 3072: ValueOf]() extends Cipher, Signing:
  type Size = BitsType
  def keySize: BitsType = valueOf[BitsType]

  def genKey(): Bytes =
    val generator = js.KeyPairGenerator.getInstance("DSA").nn
    val random = js.SecureRandom()
    generator.initialize(keySize, random)
    val keyPair = generator.generateKeyPair().nn

    val pubKey = keyPair.getPublic.nn match
      case key: js.interfaces.DSAPublicKey => key
      case key: js.PublicKey               => throw Panic(msg"public key did not have the correct type")

    keyPair.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

  def sign(data: Bytes, keyBytes: Bytes): Bytes =
    val sig = init()
    val key = keyFactory().generatePrivate(PKCS8EncodedKeySpec(keyBytes.to(Array)))
    sig.initSign(key)
    sig.update(data.to(Array))
    sig.sign().nn.immutable(using Unsafe)

  def verify(data: Bytes, signature: Bytes, keyBytes: Bytes): Boolean =
    val sig = init()
    val key = keyFactory().generatePublic(X509EncodedKeySpec(keyBytes.to(Array)))
    sig.initVerify(key)
    sig.update(data.to(Array))
    sig.verify(signature.to(Array))

  def privateToPublic(keyBytes: Bytes): Bytes =
    val key = keyFactory().generatePrivate(PKCS8EncodedKeySpec(keyBytes.to(Array))).nn match
      case key: js.interfaces.DSAPrivateKey => key
      case key: js.PrivateKey               => throw Panic(msg"private key did not have the correct type")

    val params = key.getParams.nn
    val y = params.getG.nn.modPow(key.getX, params.getP.nn)
    val spec = DSAPublicKeySpec(y, params.getP, params.getQ, params.getG)
    keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  private def init(): js.Signature = js.Signature.getInstance("DSA").nn
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("DSA").nn

object Feistel:
  def apply(subkeys: List[Int], round: (Int, Int) => Int)(input: Long): Long =
    def recur(value: Long, subkeys: List[Int]): Long = subkeys match
      case Nil => value
      case next :: more =>
        recur((value.toInt.toLong << 32) | ((value >> 32).toInt ^ round(value.toInt, next)).toLong, more)

    recur(input, subkeys)
