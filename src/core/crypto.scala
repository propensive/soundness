/*
    Gastronomy, version 0.4.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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
import javax.xml.crypto.AlgorithmMethod
import java.nio.*, charset.*

trait CryptoAlgorithm[+KeySize <: Int & Singleton]:
  def keySize: Int
  def encrypt(value: Bytes, privateKey: Bytes): Bytes
  def decrypt(message: Bytes, publicKey: Bytes): Bytes
  def genKey(): Bytes
  def privateToPublic(key: Bytes): Bytes


case class Message[+A <: CryptoAlgorithm[?]](bytes: Bytes):
  override def toString(): String = str"Message(${bytes.encode[Base64]})"

case class PublicKey[A <: CryptoAlgorithm[?]](publicBytes: Bytes):

  override def toString(): String = str"PublicKey(${publicBytes.digest[Md5].encode[Hex]})"

  def encrypt[T: ByteCodec](value: T)(using A): Message[A] =
    Message(summon[A].encrypt(summon[ByteCodec[T]].encode(value), publicBytes))
  
  def pem: Pem = Pem("PUBLIC KEY", publicBytes)

object PrivateKey:
  def generate[A <: CryptoAlgorithm[?]]()(using A): PrivateKey[A] =
    PrivateKey(summon[A].genKey())

case class PrivateKey[A <: CryptoAlgorithm[?]](private[gastronomy] val privateBytes: Bytes):

  override def toString(): String = str"PrivateKey(${privateBytes.digest[Md5].encode[Hex]})"
  def public(using A): PublicKey[A] = PublicKey(summon[A].privateToPublic(privateBytes))
  def decrypt[T: ByteCodec](message: Message[A])(using A): T = decrypt(message.bytes)
  
  def decrypt[T: ByteCodec](bytes: Bytes)(using A): T =
    summon[ByteCodec[T]].decode(summon[A].decrypt(bytes, privateBytes))
  
  def pem: Pem = Pem("PRIVATE KEY", privateBytes)

class SymmetricKey(private[gastronomy] val bytes: Bytes) extends PrivateKey(bytes)

class GastronomyException(message: String) extends Exception(str"gastronomy: $message")

case class DecodeFailure(message: String)
extends GastronomyException("could not decode the message")

case class DecryptionFailure(message: Bytes)
extends GastronomyException("could not decrypt the message")

trait ByteCodec[T]:
  def encode(value: T): Bytes
  def decode(bytes: Bytes): T exposes DecodeFailure

object ByteCodec:
  given ByteCodec[Bytes] with
    def encode(value: Bytes): Bytes = value
    def decode(bytes: Bytes): Bytes exposes DecodeFailure = bytes
   
  given ByteCodec[String] with
    def encode(value: String): Bytes = value.bytes
    def decode(bytes: Bytes): String exposes DecodeFailure =
      val buffer = ByteBuffer.wrap(bytes.unsafeMutable)
      
      try Charset.forName("UTF-8").newDecoder().decode(buffer).toString
      catch CharacterCodingException =>
        throw DecodeFailure("the message did not contain a valid UTF-8 string")

object Aes:
  given aes128: Aes[128] = Aes()
  given aes192: Aes[192] = Aes()
  given aes256: Aes[256] = Aes()

object Rsa:
  given rsa1024: Rsa[1024] = Rsa()
  given rsa2048: Rsa[2048] = Rsa()

class Aes[KS <: 128 | 192 | 256: ValueOf]() extends CryptoAlgorithm[KS]:
  def keySize: Int = valueOf[KS]
  
  private def init() = Cipher.getInstance("AES/ECB/PKCS5Padding")
  
  private def makeKey(key: Bytes): SecretKeySpec =
    SecretKeySpec(key.unsafeMutable, "AES")

  def encrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init()
    cipher.init(Cipher.ENCRYPT_MODE, makeKey(key))
    IArray.from(cipher.doFinal(message.unsafeMutable))
  
  def decrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init()
    cipher.init(Cipher.DECRYPT_MODE, makeKey(key))
    IArray.from(cipher.doFinal(message.unsafeMutable))
  
  def genKey(): Bytes =
    val keyGen = KeyGenerator.getInstance("AES")
    keyGen.init(keySize)
    
    IArray.from(keyGen.generateKey().getEncoded)
  
  def privateToPublic(key: Bytes): Bytes = key
end Aes

class Rsa[KS <: 1024 | 2048: ValueOf]() extends CryptoAlgorithm[KS]:
  def keySize: Int = valueOf[KS]

  private def init(): Cipher = Cipher.getInstance("RSA")
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("RSA")
    
  def privateToPublic(bytes: Bytes): Bytes =
    val privateKey = keyFactory().generatePrivate(PKCS8EncodedKeySpec(bytes.unsafeMutable)) match
      case key: js.interfaces.RSAPrivateCrtKey => key

    val spec = RSAPublicKeySpec(privateKey.getModulus, privateKey.getPublicExponent)
    IArray.from(keyFactory().generatePublic(spec).getEncoded)

  def decrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init()
    val privateKey = keyFactory().generatePrivate(PKCS8EncodedKeySpec(key.unsafeMutable))
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    IArray.from(cipher.doFinal(message.unsafeMutable))
  
  def encrypt(message: Bytes, key: Bytes): Bytes =
    val cipher = init()
    val publicKey = keyFactory().generatePublic(X509EncodedKeySpec(key.unsafeMutable))
    cipher.init(Cipher.ENCRYPT_MODE, publicKey)
    IArray.from(cipher.doFinal(message.unsafeMutable))
  
  def genKey(): Bytes =
    val generator = js.KeyPairGenerator.getInstance("RSA")
    generator.initialize(keySize)
    val keyPair = generator.generateKeyPair()
    IArray.from(keyPair.getPrivate.getEncoded)


case class Pem(kind: String, data: Bytes):
  def string: String = Seq(
    Seq(s"-----BEGIN $kind-----"),
    data.grouped(48).to(Seq).map(_.encode[Base64]),
    Seq(s"-----END $kind-----")
  ).flatten.join("\n")

case class PemParseError(message: String)
extends GastronomyException("could not parse PEM-encoded content")

object Pem:
  def parse(string: String): Pem exposes PemParseError =
    val lines = string.trim.cut("\n")
    
    val label = lines.head match
      case s"-----BEGIN $label-----" => label
      case _                         => throw PemParseError("the BEGIN line could not be found")
    
    lines.tail.indexWhere {
      case s"-----END $label-----" => true
      case _                       => false
    } match
      case -1  =>
        throw PemParseError("the message's END line could not be found")
      case idx =>
        try Pem(label, lines.tail.take(idx).join.decode[Base64])
        catch Exception => throw PemParseError("could not parse Base64 PEM message")




