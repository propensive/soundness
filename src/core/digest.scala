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

import wisteria.*
import rudiments.*
import gossamer.*

import scala.collection.*
import scala.compiletime.ops.int.*

import java.util.zip as juz
import java.security.*
import javax.crypto.Mac, javax.crypto.spec.SecretKeySpec
import java.util.Base64.{getEncoder as Base64Encoder, getDecoder as Base64Decoder}
import java.lang as jl

sealed trait HashScheme[Size <: Int & Singleton]

sealed trait Md5 extends HashScheme[16]

sealed trait Crc32 extends HashScheme[32]

type ByteCount[Bits] <: Int & Singleton = Bits match
  case 224 => 28
  case 256 => 32
  case 384 => 48
  case 512 => 64

sealed trait Sha2[Bits <: 224 | 256 | 384 | 512] extends HashScheme[ByteCount[Bits]]
sealed trait Sha1 extends HashScheme[20]
sealed trait Sha384 extends HashScheme[48]
sealed trait Sha512 extends HashScheme[64]

object Crc32:
  given HashFunction[Crc32] = Crc32HashFunction

object Md5:
  given HashFunction[Md5] = MdHashFunction(t"MD5", t"HmacMD5")

object Sha1:
  given HashFunction[Sha1] = MdHashFunction(t"SHA1", t"HmacSHA1")

object Sha2:
  given sha2[Bits <: 224 | 256 | 384 | 512: ValueOf]: HashFunction[Sha2[Bits]] =
    MdHashFunction(t"SHA-${valueOf[Bits]}", t"HmacSHA${valueOf[Bits]}")

trait Encodable:
  val bytes: Bytes
  def encode[ES <: EncodingScheme: ByteEncoder]: Text = bytes.encode[ES]

object Hmac:
  given Show[Hmac[?]] = hmac => t"Hmac(${hmac.bytes.encode[Base64]})"

case class Hmac[A <: HashScheme[?]](bytes: Bytes) extends Encodable, Shown[Hmac[?]]

trait HashFunction[A <: HashScheme[?]]:
  def name: Text
  def hmacName: Text
  def init: DigestAccumulator
  def initHmac: Mac

case class MdHashFunction[A <: HashScheme[?]](name: Text, hmacName: Text) extends HashFunction[A]:
  def init: DigestAccumulator = new MessageDigestAccumulator(MessageDigest.getInstance(name.s).nn)
  def initHmac: Mac = Mac.getInstance(hmacName.s).nn

case object Crc32HashFunction extends HashFunction[Crc32]:
  def init: DigestAccumulator = new DigestAccumulator:
    private val state: juz.CRC32 = juz.CRC32()
    def append(bytes: Bytes): Unit = state.update(bytes.asInstanceOf[Array[Byte]])
    
    def digest(): Bytes =
      val int = state.getValue()
      IArray[Byte]((int >> 24).toByte, (int >> 16).toByte, (int >> 8).toByte, int.toByte)
  def name: Text = t"CRC32"
  def hmacName: Text = t"HMAC-CRC32"
  def initHmac: Mac = ???
    

object Digest:
  given Show[Digest[?]] = digest => t"Digest(${digest.bytes.encode[Base64]})"

case class Digest[A <: HashScheme[?]](bytes: Bytes) extends Encodable, Shown[Digest[?]]

object Hashable extends Derivation[Hashable]:
  def join[T](caseClass: CaseClass[Hashable, T]): Hashable[T] =
    (acc, value) => caseClass.params.foreach:
      param => param.typeclass.digest(acc, param.deref(value))

  def split[T](sealedTrait: SealedTrait[Hashable, T]): Hashable[T] =
    (acc, value) => sealedTrait.choose(value):
      subtype =>
        summon[Hashable[Int]].digest(acc, sealedTrait.subtypes.indexOf(subtype))
        subtype.typeclass.digest(acc, subtype.cast(value))
    
  given[T: Hashable]: Hashable[Traversable[T]] =
    (acc, xs) => xs.foreach(summon[Hashable[T]].digest(acc, _))
  
  given Hashable[Int] =
    (acc, n) => acc.append((24 to 0 by -8).map(n >> _).map(_.toByte).toArray.unsafeImmutable)
  
  given Hashable[Long] =
    (acc, n) => acc.append((52 to 0 by -8).map(n >> _).map(_.toByte).toArray.unsafeImmutable)
  
  given Hashable[Double] =
    (acc, n) => summon[Hashable[Long]].digest(acc, jl.Double.doubleToRawLongBits(n))
  
  given Hashable[Float] =
    (acc, n) => summon[Hashable[Int]].digest(acc, jl.Float.floatToRawIntBits(n))
  
  given Hashable[Boolean] = (acc, n) => acc.append(IArray(if n then 1.toByte else 0.toByte))
  given Hashable[Byte] = (acc, n) => acc.append(IArray(n))
  given Hashable[Short] = (acc, n) => acc.append(IArray((n >> 8).toByte, n.toByte))
  given Hashable[Char] = (acc, n) => acc.append(IArray((n >> 8).toByte, n.toByte))
  given Hashable[Text] = (acc, s) => acc.append(s.bytes)
  given Hashable[Bytes] = _.append(_)
  given Hashable[Iterable[Bytes]] = (acc, stream) => stream.foreach(acc.append(_))
  given Hashable[Digest[?]] = (acc, d) => acc.append(d.bytes)

trait Hashable[-T]:
  def digest(acc: DigestAccumulator, value: T): Unit

case class Digester(run: DigestAccumulator => Unit):
  def apply[A <: HashScheme[?]: HashFunction]: Digest[A] =
    val acc = summon[HashFunction[A]].init
    run(acc)
    Digest(acc.digest())
  
  def digest[T: Hashable](value: T): Digester =
    Digester:
      acc =>
        run(acc)
        summon[Hashable[T]].digest(acc, value)

trait DigestAccumulator:
  def append(bytes: Bytes): Unit
  def digest(): Bytes

class MessageDigestAccumulator(md: MessageDigest) extends DigestAccumulator:
  private val messageDigest: MessageDigest = md
  def append(bytes: Bytes): Unit = messageDigest.update(bytes.unsafeMutable)
  def digest(): Bytes = messageDigest.digest.nn.unsafeImmutable

trait EncodingScheme
trait Base64 extends EncodingScheme
trait Base64Url extends EncodingScheme
trait Base32 extends EncodingScheme
trait Hex extends EncodingScheme
trait Binary extends EncodingScheme

object ByteEncoder:
  private val HexLookup: Bytes = IArray.from(t"0123456789ABCDEF".bytes)

  given ByteEncoder[Hex] = bytes =>
    val array = new Array[Byte](bytes.length*2)
    bytes.indices.foreach:
      idx =>
        array(2*idx) = HexLookup((bytes(idx) >> 4) & 0xf)
        array(2*idx + 1) = HexLookup(bytes(idx) & 0xf)
    
    Text(String(array, "UTF-8"))

  given ByteEncoder[Base64] = bytes => Text(Base64Encoder.nn.encodeToString(bytes.to(Array)).nn)
  
  given ByteEncoder[Binary] = bytes =>
    val buf = StringBuilder()
    bytes.foreach:
      byte => buf.add(Integer.toBinaryString(byte).nn.show.fit(8, Rtl, '0'))
    buf.text

  given ByteEncoder[Base64Url] = bytes =>
    Text(Base64Encoder.nn.encodeToString(bytes.to(Array)).nn)
      .tr('+', '-')
      .tr('/', '_')
      .upto(_ != '=')

trait ByteDecoder[ES <: EncodingScheme]:
  def decode(value: Text): Bytes

trait ByteEncoder[ES <: EncodingScheme]:
  def encode(bytes: Bytes): Text

object ByteDecoder:
  given ByteDecoder[Base64] = value => Base64Decoder.nn.decode(value.s).nn.unsafeImmutable
  
  given ByteDecoder[Hex] = value =>
    import java.lang.Character.digit
    val data = Array.fill[Byte](value.length/2)(0)
    
    (0 until value.length by 2).foreach:
      i =>
        try data(i/2) = ((digit(value(i), 16) << 4) + digit(value(i + 1), 16)).toByte
        catch case e: OutOfRangeError => throw Impossible("every accessed element should be within range")

    data.unsafeImmutable

extension (value: Text)
  def decode[T <: EncodingScheme: ByteDecoder]: Bytes = summon[ByteDecoder[T]].decode(value)

extension [T](value: T)
  def digest[A <: HashScheme[?]: HashFunction](using Hashable[T]): Digest[A] =
    val digester = Digester(summon[Hashable[T]].digest(_, value))
    digester.apply

  def hmac[A <: HashScheme[?]: HashFunction](key: Bytes)(using ByteCodec[T]): Hmac[A] =
    val mac = summon[HashFunction[A]].initHmac
    mac.init(SecretKeySpec(key.to(Array), summon[HashFunction[A]].name.s))
    Hmac(mac.doFinal(summon[ByteCodec[T]].encode(value).unsafeMutable).nn.unsafeImmutable)

extension (bytes: Bytes)
  def encode[E <: EncodingScheme: ByteEncoder]: Text = summon[ByteEncoder[E]].encode(bytes)
