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

import javax.crypto.*, javax.crypto.spec.*
import java.security as js, js.spec.*

import rudiments.*
import gossamer.*

import java.nio.*, charset.*

trait CryptoAlgorithm[+KeySize <: Int & Singleton]:
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

case class Message[+A <: CryptoAlgorithm[?]](bytes: Bytes) extends Encodable:
  override def toString(): String = t"Message(${bytes.encode[Base64]})".s

case class Signature[+A <: CryptoAlgorithm[?]](bytes: Bytes) extends Encodable:
  override def toString(): String = t"Signature(${bytes.encode[Base64]})".s

object RevealSecretKey

case class PublicKey[A <: CryptoAlgorithm[?]](bytes: Bytes):
  override def toString(): String = t"PublicKey(${bytes.encode[Hex]})".s

  def encrypt[T: ByteCodec](value: T)(using A & Encryption): Message[A] =
    Message(summon[A].encrypt(summon[ByteCodec[T]].encode(value), bytes))
  
  def verify[T: ByteCodec](value: T, signature: Signature[A])(using A & Signing): Boolean =
    summon[A].verify(summon[ByteCodec[T]].encode(value), signature.bytes, bytes)

  def pem: Pem = Pem(t"PUBLIC KEY", bytes)

object PrivateKey:
  def generate[A <: CryptoAlgorithm[?]]()(using A): PrivateKey[A] =
    PrivateKey(summon[A].genKey())

case class PrivateKey[A <: CryptoAlgorithm[?]](private[gastronomy] val privateBytes: Bytes):
  override def toString(): String =
    t"PrivateKey(${privateBytes.digest[Sha2[256]].encode[Base64]})".s
  
  def public(using A): PublicKey[A] = PublicKey(summon[A].privateToPublic(privateBytes))
  
  inline def decrypt[T: ByteCodec](message: Message[A])(using A & Encryption): T throws DecodeFailure =
    decrypt(message.bytes)
  
  inline def decrypt[T: ByteCodec](bytes: Bytes)(using A & Encryption): T throws DecodeFailure =
    summon[ByteCodec[T]].decode(summon[A].decrypt(bytes, privateBytes))
  
  inline def sign[T: ByteCodec](value: T)(using A & Signing): Signature[A] =
    Signature(summon[A].sign(summon[ByteCodec[T]].encode(value), privateBytes))
  
  def pem(reveal: RevealSecretKey.type): Pem = Pem(t"PRIVATE KEY", privateBytes)

object SymmetricKey:
  def generate[A <: CryptoAlgorithm[?] & Symmetric]()(using A): SymmetricKey[A] =
    SymmetricKey(summon[A].genKey())

class SymmetricKey[A <: CryptoAlgorithm[?]](private[gastronomy] val bytes: Bytes)
extends PrivateKey[A](bytes):
  def encrypt[T: ByteCodec](value: T)(using A & Encryption): Message[A] = public.encrypt(value)
  
  def verify[T: ByteCodec](value: T, signature: Signature[A])(using A & Signing): Boolean =
    public.verify(value, signature)

class GastronomyException(message: Text) extends Exception(t"gastronomy: $message".s)

case class DecodeFailure(message: Text)
extends GastronomyException(t"could not decode the message")

case class DecryptionFailure(message: Bytes)
extends GastronomyException(t"could not decrypt the message")

trait ByteCodec[T]:
  def encode(value: T): Bytes
  def decode(bytes: Bytes): T throws DecodeFailure

object ByteCodec:
  given ByteCodec[Bytes] with
    def encode(value: Bytes): Bytes = value
    def decode(bytes: Bytes): Bytes throws DecodeFailure = bytes
   
  given ByteCodec[Text] with
    def encode(value: Text): Bytes = value.bytes
    def decode(bytes: Bytes): Text throws DecodeFailure =
      val buffer = ByteBuffer.wrap(bytes.unsafeMutable)
      
      try Text(Charset.forName("UTF-8").nn.newDecoder().nn.decode(buffer).toString)
      catch CharacterCodingException =>
        throw DecodeFailure(t"the message did not contain a valid UTF-8 string")

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
    SecretKeySpec(key.unsafeMutable, "AES")

  def encrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    cipher.init(Cipher.ENCRYPT_MODE, makeKey(key))
    IArray.from(cipher.doFinal(message.unsafeMutable).nn)
  
  def decrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    cipher.init(Cipher.DECRYPT_MODE, makeKey(key))
    IArray.from(cipher.doFinal(message.unsafeMutable).nn)
  
  def genKey(): Bytes =
    val keyGen = KeyGenerator.getInstance("AES").nn
    keyGen.init(keySize)
    
    IArray.from(keyGen.generateKey().nn.getEncoded.nn)
  
  def privateToPublic(key: Bytes): Bytes = key
end Aes

class Rsa[KS <: 1024 | 2048: ValueOf]() extends CryptoAlgorithm[KS], Encryption:
  def keySize: KS = valueOf[KS]
    
  def privateToPublic(bytes: Bytes): Bytes =
    val privateKey = keyFactory().generatePrivate(PKCS8EncodedKeySpec(bytes.unsafeMutable)).nn match
      case key: js.interfaces.RSAPrivateCrtKey =>
        key
      case key: js.PrivateKey =>
        throw Impossible("public key did not have the correct type")

    val spec = RSAPublicKeySpec(privateKey.getModulus, privateKey.getPublicExponent)
    IArray.from(keyFactory().generatePublic(spec).nn.getEncoded.nn)

  def decrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    val privateKey = keyFactory().generatePrivate(PKCS8EncodedKeySpec(key.unsafeMutable))
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    IArray.from(cipher.doFinal(message.unsafeMutable).nn)
  
  def encrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    val publicKey = keyFactory().generatePublic(X509EncodedKeySpec(key.unsafeMutable))
    cipher.init(Cipher.ENCRYPT_MODE, publicKey)
    IArray.from(cipher.doFinal(message.unsafeMutable).nn)
  
  def genKey(): Bytes =
    val generator = js.KeyPairGenerator.getInstance("RSA").nn
    generator.initialize(keySize)
    val keyPair = generator.generateKeyPair().nn
    IArray.from(keyPair.getPrivate.nn.getEncoded.nn)

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
        throw Impossible("public key did not have the correct type")
    
    IArray.from(keyPair.getPrivate.nn.getEncoded.nn)

  def sign(data: Bytes, keyBytes: Bytes): Bytes =
    val sig = init()
    val key = keyFactory().generatePrivate(PKCS8EncodedKeySpec(keyBytes.to(Array)))
    sig.initSign(key)
    sig.update(data.to(Array))
    IArray.from(sig.sign().nn)

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
        throw Impossible("private key did not have the correct type")

    val params = key.getParams.nn
    val y = params.getG.nn.modPow(key.getX, params.getP.nn)
    val spec = DSAPublicKeySpec(y, params.getP, params.getQ, params.getG)
    IArray.from(keyFactory().generatePublic(spec).nn.getEncoded.nn)

  private def init(): js.Signature = js.Signature.getInstance("DSA").nn
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("DSA").nn
end Dsa

case class PemParseError(message: Text)
extends GastronomyException(t"could not parse PEM-encoded content")