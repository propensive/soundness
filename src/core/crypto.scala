package gastronomy

import javax.crypto.*, spec.*
import java.security.*

import rudiments.*

case class SymmetricKey(bytes: IArray[Byte])

case class CanNotDecode() extends Exception("gastronomy: cannot decode value")

extension [T](value: T)
  def encrypt(key: SymmetricKey)(using ByteCodec[T]) =
    Aes.encrypt(summon[ByteCodec[T]].encode(value), key.bytes)

extension (bytes: IArray[Byte])
  def decrypt[T: ByteCodec](key: SymmetricKey): T =
    summon[ByteCodec[T]].decode(Aes.decrypt(bytes, key.bytes)).getOrElse(throw CanNotDecode())

trait ByteCodec[T]:
  def encode(value: T): IArray[Byte]
  def decode(bytes: IArray[Byte]): Option[T]

object ByteCodec:
  given ByteCodec[IArray[Byte]] with
    def encode(value: IArray[Byte]): IArray[Byte] = value
    def decode(bytes: IArray[Byte]): Option[IArray[Byte]] = Some(bytes)
   
  given ByteCodec[String] with
    def encode(value: String): IArray[Byte] = value.bytes
    def decode(bytes: IArray[Byte]): Option[String] = Some(String(bytes.asInstanceOf[Array[Byte]]))

object Aes:
  private def init() = Cipher.getInstance("AES/ECB/PKCS5Padding")
  private def makeKey(key: IArray[Byte]): SecretKeySpec = SecretKeySpec(key.asInstanceOf[Array[Byte]], "AES")

  def encrypt(message: IArray[Byte], key: IArray[Byte]): IArray[Byte] =
    val cipher = init()
    cipher.init(Cipher.ENCRYPT_MODE, makeKey(key))
    cipher.doFinal(message.asInstanceOf[Array[Byte]]).asInstanceOf[IArray[Byte]]
  
  def decrypt(message: IArray[Byte], key: IArray[Byte]): IArray[Byte] =
    val cipher = init()
    cipher.init(Cipher.DECRYPT_MODE, makeKey(key))
    cipher.doFinal(message.asInstanceOf[Array[Byte]]).asInstanceOf[IArray[Byte]]