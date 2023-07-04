/*
    Gastronomy, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import javax.crypto.*, javax.crypto.spec.*
import java.security as js, js.spec.*

import rudiments.*
import digression.*
import gossamer.*
import spectacular.*
import hieroglyph.*

import java.nio.*, charset.*

trait CryptoAlgorithm[+KeySize <: Nat]:
  def keySize: KeySize
  def privateToPublic(key: Bytes): Bytes
  def genKey(): Bytes

trait Encryption:
  def encrypt(value: Bytes, privateKey: Bytes): Bytes
  def decrypt(message: Bytes, publicKey: Bytes): Bytes

trait Signing:
  def sign(data: Bytes, privateKey: Bytes): Bytes
  def verify(data: Bytes, signature: Bytes, publicKey: Bytes): Boolean

trait Symmetric

object MessageData:
  given Show[MessageData[?]] = msg => t"MessageData(${msg.bytes.encode[Base64]})"

case class MessageData[+A <: CryptoAlgorithm[?]](bytes: Bytes) extends Encodable, Shown[MessageData[?]]

object Signature:
  given Show[Signature[?]] = sig => t"Signature(${sig.bytes.encode[Base64]})"

case class Signature[+A <: CryptoAlgorithm[?]](bytes: Bytes) extends Encodable, Shown[Signature[?]]

object ExposeSecretKey

object PublicKey:
  given Show[PublicKey[?]] = key => t"PublicKey(${key.bytes.encode[Hex]})"

case class PublicKey[A <: CryptoAlgorithm[?]](bytes: Bytes) extends Shown[PublicKey[?]]:
  def encrypt[T: ByteCodec](value: T)(using A & Encryption): MessageData[A] =
    MessageData(summon[A].encrypt(summon[ByteCodec[T]].encode(value), bytes))
  
  def verify[T: ByteCodec](value: T, signature: Signature[A])(using A & Signing): Boolean =
    summon[A].verify(summon[ByteCodec[T]].encode(value), signature.bytes, bytes)

  def pem: Pem = Pem(t"PUBLIC KEY", bytes)

object PrivateKey:
  def generate[A <: CryptoAlgorithm[?]]()(using A): PrivateKey[A] =
    PrivateKey(summon[A].genKey())

  given Show[PrivateKey[?]] =
    key => t"PrivateKey(${key.privateBytes.digest[Sha2[256]].encode[Base64]})"

case class PrivateKey[A <: CryptoAlgorithm[?]](private[gastronomy] val privateBytes: Bytes)
extends Shown[PrivateKey[?]]:
  def public(using A): PublicKey[A] = PublicKey(summon[A].privateToPublic(privateBytes))
  
  inline def decrypt[T: ByteCodec](message: MessageData[A])(using A & Encryption): T throws DecodeError =
    decrypt(message.bytes)
  
  inline def decrypt[T: ByteCodec](bytes: Bytes)(using A & Encryption): T throws DecodeError =
    summon[ByteCodec[T]].decode(summon[A].decrypt(bytes, privateBytes))
  
  inline def sign[T: ByteCodec](value: T)(using A & Signing): Signature[A] =
    Signature(summon[A].sign(summon[ByteCodec[T]].encode(value), privateBytes))
  
  def pem(reveal: ExposeSecretKey.type): Pem = Pem(t"PRIVATE KEY", privateBytes)

object SymmetricKey:
  def generate[A <: CryptoAlgorithm[?] & Symmetric]()(using A): SymmetricKey[A] =
    SymmetricKey(summon[A].genKey())

class SymmetricKey[A <: CryptoAlgorithm[?]](private[gastronomy] val bytes: Bytes)
extends PrivateKey[A](bytes):
  def encrypt[T: ByteCodec](value: T)(using A & Encryption): MessageData[A] = public.encrypt(value)
  
  def verify[T: ByteCodec](value: T, signature: Signature[A])(using A & Signing): Boolean =
    public.verify(value, signature)

case class DecodeError(detail: Text) extends Error(msg"could not decode the encrypted data: $detail")

trait ByteCodec[T]:
  def encode(value: T): Bytes
  def decode(bytes: Bytes): T throws DecodeError

object ByteCodec:
  given ByteCodec[Bytes] with
    def encode(value: Bytes): Bytes = value
    def decode(bytes: Bytes): Bytes throws DecodeError = bytes
   
  given (using CharDecoder, CharEncoder): ByteCodec[Text] with
    def encode(value: Text): Bytes = value.bytes
    def decode(bytes: Bytes): Text throws DecodeError =
      val buffer = ByteBuffer.wrap(bytes.mutable(using Unsafe))
      
      try Charset.forName("UTF-8").nn.newDecoder().nn.decode(buffer).toString.show
      catch CharacterCodingException =>
        throw DecodeError(t"the message did not contain a valid UTF-8 string")

object Aes:
  given aes[I <: 128 | 192 | 256: ValueOf]: Aes[I] = Aes()

object Rsa:
  given rsa[I <: 1024 | 2048: ValueOf]: Rsa[I] = Rsa()

object Dsa:
  given dsa[I <: 512 | 1024 | 2048 | 3072: ValueOf]: Dsa[I] = Dsa()

class Aes[KS <: 128 | 192 | 256: ValueOf]() extends CryptoAlgorithm[KS], Encryption, Symmetric:
  def keySize: KS = valueOf[KS]
  
  private def init() = Cipher.getInstance("AES/ECB/PKCS5Padding")
  
  private def makeKey(key: Bytes): SecretKeySpec =
    SecretKeySpec(key.mutable(using Unsafe), "AES")

  def encrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    cipher.init(Cipher.ENCRYPT_MODE, makeKey(key))
    cipher.doFinal(message.mutable(using Unsafe)).nn.immutable(using Unsafe)
  
  def decrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    cipher.init(Cipher.DECRYPT_MODE, makeKey(key))
    cipher.doFinal(message.mutable(using Unsafe)).nn.immutable(using Unsafe)
  
  def genKey(): Bytes =
    val keyGen = KeyGenerator.getInstance("AES").nn
    keyGen.init(keySize)
    
    keyGen.generateKey().nn.getEncoded.nn.immutable(using Unsafe)
  
  def privateToPublic(key: Bytes): Bytes = key
end Aes

class Rsa[KS <: 1024 | 2048: ValueOf]() extends CryptoAlgorithm[KS], Encryption:
  def keySize: KS = valueOf[KS]
    
  def privateToPublic(bytes: Bytes): Bytes =
    val privateKey = keyFactory().generatePrivate(PKCS8EncodedKeySpec(bytes.mutable(using Unsafe))).nn match
      case key: js.interfaces.RSAPrivateCrtKey =>
        key
      case key: js.PrivateKey =>
        throw Mistake("public key did not have the correct type")

    val spec = RSAPublicKeySpec(privateKey.getModulus, privateKey.getPublicExponent)
    keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  def decrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    val privateKey = keyFactory().generatePrivate(PKCS8EncodedKeySpec(key.mutable(using Unsafe)))
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    cipher.doFinal(message.mutable(using Unsafe)).nn.immutable(using Unsafe)
  
  def encrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    val publicKey = keyFactory().generatePublic(X509EncodedKeySpec(key.mutable(using Unsafe)))
    cipher.init(Cipher.ENCRYPT_MODE, publicKey)
    cipher.doFinal(message.mutable(using Unsafe)).nn.immutable(using Unsafe)
  
  def genKey(): Bytes =
    val generator = js.KeyPairGenerator.getInstance("RSA").nn
    generator.initialize(keySize)
    val keyPair = generator.generateKeyPair().nn
    keyPair.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

  private def init(): Cipher = Cipher.getInstance("RSA").nn
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("RSA").nn
end Rsa

class Dsa[KS <: 512 | 1024 | 2048 | 3072: ValueOf]() extends CryptoAlgorithm[KS], Signing:
  def keySize: KS = valueOf[KS]

  def genKey(): Bytes =
    val generator = js.KeyPairGenerator.getInstance("DSA").nn
    val random = js.SecureRandom()
    generator.initialize(keySize, random)
    val keyPair = generator.generateKeyPair().nn
    
    val pubKey = keyPair.getPublic.nn match
      case key: js.interfaces.DSAPublicKey =>
        key
      
      case key: js.PublicKey =>
        throw Mistake("public key did not have the correct type")
    
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
      case key: js.interfaces.DSAPrivateKey =>
        key
      
      case key: js.PrivateKey =>
        throw Mistake("private key did not have the correct type")

    val params = key.getParams.nn
    val y = params.getG.nn.modPow(key.getX, params.getP.nn)
    val spec = DSAPublicKeySpec(y, params.getP, params.getQ, params.getG)
    keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  private def init(): js.Signature = js.Signature.getInstance("DSA").nn
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("DSA").nn
end Dsa

case class PemParseError(detail: Text) extends Error(msg"could not parse PEM-encoded content: $detail")
