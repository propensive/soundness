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

import rudiments.*
import vacuous.*
import fulminate.*
import wisteria.*
import contingency.*
import turbulence.*
import gossamer.*
import anticipation.*
import spectacular.*
import hieroglyph.*, textMetrics.uniform

import scala.collection.*
import scala.compiletime.*, ops.int.*

import java.util as ju
import ju.zip as juz
import java.security.*
import javax.crypto.Mac, javax.crypto.spec.SecretKeySpec
import ju.Base64.getDecoder as Base64Decoder
import java.lang as jl

case class Alphabet[EncodingType <: BinaryEncoding](chars: Text, padding: Boolean):
  def apply(index: Int): Char = chars.s.charAt(index)

sealed trait HashScheme:
  type Of <: Nat
  type Size = Of

sealed trait Md5 extends HashScheme:
  type Bits = 16

sealed trait Crc32 extends HashScheme:
  type Bits = 32

infix type of [Type <: { type Of }, OfType] = Type { type Of = OfType }

type ByteCount[BitsType] <: Nat = BitsType match
  case 224 => 28
  case 256 => 32
  case 384 => 48
  case 512 => 64

sealed trait Sha2[BitsType <: 224 | 256 | 384 | 512] extends HashScheme:
  type Bits = ByteCount[BitsType]

sealed trait Sha1 extends HashScheme:
  type Bits = 20

sealed trait Sha384 extends HashScheme:
  type Bits = 48

sealed trait Sha512 extends HashScheme:
  type Bits = 64

object Crc32:
  given HashFunction of Crc32 as hashFunction:
    def init(): DigestAccumulator = new DigestAccumulator:
      private val state: juz.CRC32 = juz.CRC32()
      def append(bytes: Bytes): Unit = state.update(bytes.mutable(using Unsafe))

      def digest(): Bytes =
        val int = state.getValue()
        IArray[Byte]((int >> 24).toByte, (int >> 16).toByte, (int >> 8).toByte, int.toByte)

    def name: Text = t"CRC32"
    def hmacName: Text = t"HMAC-CRC32"
    def hmac0: Mac = throw Panic(msg"this has not been implemented")

trait JavaHashFunction extends HashFunction:
  def init(): DigestAccumulator = new MessageDigestAccumulator(MessageDigest.getInstance(name.s).nn)
  def hmac0: Mac = Mac.getInstance(hmacName.s).nn

object Md5:
  given JavaHashFunction of Md5 as hashFunction:
    val name: Text = t"MD5"
    val hmacName: Text = t"HmacMD5"

object Sha1:
  given JavaHashFunction of Sha1 as hashFunction:
    val name: Text = t"SHA1"
    val hmacName: Text = t"HmacSHA1"

object Sha2:
  given [BitsType <: 224 | 256 | 384 | 512: ValueOf]
      => JavaHashFunction of Sha2[BitsType] as hashFunction:
    private val bits: Int = valueOf[BitsType]
    val name: Text = t"SHA-$bits"
    val hmacName: Text = t"HmacSHA$bits"

trait Encodable:
  val bytes: Bytes

  def encodeAs[SchemeType <: BinaryEncoding](using encodable: BinaryEncodable in SchemeType): Text =
    bytes.encodeAs[SchemeType]

object Hmac:
  def apply[SchemeType <: HashScheme](bytes: Bytes) = new Hmac(bytes):
    type Of = SchemeType

  given [HmacType <: HashScheme](using Alphabet[Base64]) => Hmac of HmacType is Showable = hmac =>
    t"Hmac(${hmac.bytes.encodeAs[Base64]})"

class Hmac(val bytes: Bytes):
  type Of <: HashScheme

trait HashFunction:
  type Of <: HashScheme
  def name: Text
  def hmacName: Text
  def init(): DigestAccumulator
  def hmac0: Mac


object Digest:
  def apply[HashType <: HashScheme](bytes: Bytes): Digest of HashType = new Digest(bytes):
    type Of = HashType

  given [DigestType <: HashScheme](using Alphabet[Base64]) => Digest of DigestType is Showable =
    _.bytes.encodeAs[Base64]

class Digest(val bytes: Bytes):
  type Of <: HashScheme
  override def equals(that: Any) = that.asMatchable match
    case digest: Digest => bytes.sameElements(digest.bytes)
    case _              => false

  override def hashCode: Int = ju.Arrays.hashCode(bytes.mutable(using Unsafe): Array[Byte])

object Digestible extends Derivable[Digestible]:
  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Digestible =
    (accumulator, value) => fields(value):
      [FieldType] => field => context.digest(accumulator, field)

  inline def split[DerivationType: SumReflection]: DerivationType is Digestible =
    (accumulator, value) =>
      variant(value):
        [VariantType <: DerivationType] => variant =>
          int.digest(accumulator, index)
          context.digest(accumulator, variant)

  given [ValueType: Digestible](using util.NotGiven[Unset.type <:< ValueType])
      => Optional[ValueType] is Digestible as optional =
    (acc, value) => value.let(ValueType.digest(acc, _))

  given [ValueType: Digestible] => Iterable[ValueType] is Digestible as iterable =
    (accumulator, iterable) => iterable.each(ValueType.digest(accumulator, _))

  given [ValueType: Digestible] => LazyList[ValueType] is Digestible as lazyList =
    (accumulator, iterable) => iterable.each(ValueType.digest(accumulator, _))

  given Int is Digestible as int = (accumulator, value) =>
    accumulator.append((24 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given Long is Digestible as long = (accumulator, value) =>
    accumulator.append((52 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given Double is Digestible =
    (accumulator, value) => long.digest(accumulator, jl.Double.doubleToRawLongBits(value))

  given Float is Digestible =
    (accumulator, value) => int.digest(accumulator, jl.Float.floatToRawIntBits(value))

  given Boolean is Digestible = (acc, n) => acc.append(IArray(if n then 1.toByte else 0.toByte))
  given Byte is Digestible = (acc, n) => acc.append(IArray(n))
  given Short is Digestible = (acc, n) => acc.append(IArray((n >> 8).toByte, n.toByte))
  given Char is Digestible = (acc, n) => acc.append(IArray((n >> 8).toByte, n.toByte))
  given Text is Digestible = (acc, s) => acc.append(s.bytes(using charEncoders.utf8))
  given Bytes is Digestible = _.append(_)
  given Digest is Digestible = (acc, d) => acc.append(d.bytes)

trait Digestible:
  type Self
  def digest(acc: DigestAccumulator, value: Self): Unit

case class Digester(run: DigestAccumulator => Unit):
  def apply[HashType <: HashScheme](using function: HashFunction of HashType): Digest of HashType =
    function.init().pipe: accumulator =>
      run(accumulator)
      Digest[HashType](accumulator.digest())

  def digest[ValueType: Digestible](value: ValueType): Digester = Digester:
    accumulator =>
      run(accumulator)
      ValueType.digest(accumulator, value)

trait DigestAccumulator:
  def append(bytes: Bytes): Unit
  def digest(): Bytes

class MessageDigestAccumulator(md: MessageDigest) extends DigestAccumulator:
  private val messageDigest: MessageDigest = md
  def append(bytes: Bytes): Unit = messageDigest.update(bytes.mutable(using Unsafe))
  def digest(): Bytes = messageDigest.digest.nn.immutable(using Unsafe)

erased trait BinaryEncoding
erased trait Base64 extends BinaryEncoding
erased trait Base32 extends BinaryEncoding
erased trait Hex extends BinaryEncoding
erased trait Binary extends BinaryEncoding

package hashFunctions:
  given HashFunction of Crc32 as crc32 = Crc32.hashFunction
  given HashFunction of Md5 as md5 = Md5.hashFunction
  given HashFunction of Sha1 as sha1 = Sha1.hashFunction

  given [BitsType <: 224 | 256 | 384 | 512: ValueOf] => HashFunction of Sha2[BitsType] as sha2 =
    Sha2.hashFunction[BitsType]

package alphabets:
  package base32:
    given Alphabet[Base32] as default = Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=", true)
    given Alphabet[Base32] as zBase32 = Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769=", true)
    given Alphabet[Base32] as zBase32Unpadded = Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769", false)

  package hex:
    given Alphabet[Hex] as upperCase = Alphabet(t"0123456789ABCDEF", false)
    given Alphabet[Hex] as lowerCase = Alphabet(t"0123456789abcdef", false)
    given Alphabet[Hex] as bioctal = Alphabet(t"01234567cjzwfsbv", false)

  package base64:
    given Alphabet[Base64] as standard =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", true)

    given Alphabet[Base64] as unpadded =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", false)

    given Alphabet[Base64] as url =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_", false)

    given Alphabet[Base64] as xml =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-.", true)

    given Alphabet[Base64] as imap =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,", false)

    given Alphabet[Base64] as yui =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._", false)

    given Alphabet[Base64] as radix64 =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", true)

    given Alphabet[Base64] as bcrypt =
      Alphabet(t"./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", false)

    given Alphabet[Base64] as sasl =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,", false)

    given Alphabet[Base64] as Uuencoding =
      Alphabet(t"""!"#$$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_""", false)


object BinaryEncodable:
  private val HexLookup: Bytes = IArray.from(t"0123456789ABCDEF".bytes(using charEncoders.ascii))

  given (using alphabet: Alphabet[Hex]) => BinaryEncodable in Hex:
    def encode(bytes: Bytes): Text =
      val array = new Array[Char](bytes.length*2)

      bytes.indices.each: index =>
        array(2*index) = alphabet((bytes(index) >> 4) & 0xf)
        array(2*index + 1) = alphabet(bytes(index) & 0xf)

      Text(String(array))

  given (using alphabet: Alphabet[Base32]) => BinaryEncodable in Base32:
    def encode(bytes: Bytes): Text = Text.construct:
      @tailrec
      def recur(acc: Int, index: Int, unused: Int): Unit =
        append(alphabet((acc >> 11) & 0x1f))

        if index >= bytes.length then
          append(alphabet((acc >> 6) & 0x1f))
          if unused == 5 then append(alphabet((acc >> 1) & 0x1f))
          if index%8 != 0 then for i <- 0 until (5 - index%8) do append(alphabet(32))

        else
          if unused >= 8 then recur((acc << 5) + (bytes(index) << (unused - 3)), index + 1, unused - 3)
          else recur(acc << 5, index, unused + 5)

      if bytes.isEmpty then t"" else recur(bytes.head.toInt << 8, 1, 8)

  given (using alphabet: Alphabet[Base64]) => BinaryEncodable in Base64:
    def encode(bytes: Bytes): Text = Text.construct:
      @tailrec
      def recur(i: Int = 0): Int = if i >= bytes.length - 2 then i else
        val word = ((bytes(i) & 0xff) << 16) | ((bytes(i + 1) & 0xff) << 8) | (bytes(i + 2) & 0xff)
        append(alphabet((word >> 18) & 0x3f))
        append(alphabet((word >> 12) & 0x3f))
        append(alphabet((word >> 6) & 0x3f))
        append(alphabet(word & 0x3f))
        recur(i + 3)

      val last = recur()
      if last < bytes.length then
        val word = (bytes(last) & 0xff) << 16
        append(alphabet((word >> 18) & 0x3f))
        if last + 1 < bytes.length then
          val word2 = word | ((bytes(last + 1) & 0xff) << 8)
          append(alphabet((word2 >> 12) & 0x3f))
          append(alphabet((word2 >> 6) & 0x3f))
          if alphabet.padding then append(alphabet(64))
        else
          append(alphabet((word >> 12) & 0x3f))
          if alphabet.padding then
            append(alphabet(64))
            append(alphabet(64))

  given BinaryEncodable in Binary:
    def encode(bytes: Bytes): Text = Text.construct:
      bytes.each:
        byte => append(Integer.toBinaryString(byte).nn.show.fit(8, Rtl, '0'))

trait BinaryDecodable:
  type In <: BinaryEncoding
  def decode(value: Text): Bytes

trait BinaryEncodable:
  type In <: BinaryEncoding
  def encode(bytes: Bytes): Text

object BinaryDecodable:
  given (using Errant[CryptoError]) => BinaryDecodable in Base64:
    def decode(value: Text): Bytes =
      try Base64Decoder.nn.decode(value.s).nn.immutable(using Unsafe)
      catch case _: IllegalArgumentException => abort(CryptoError(t"an invalid BASE-64 character found"))

  given BinaryDecodable in Hex:
    def decode(value: Text): Bytes =
      import java.lang.Character.digit
      val data = Array.fill[Byte](value.length/2)(0)

      (0 until value.length by 2).each: i =>
        data(i/2) = unsafely(((digit(value.at(i).vouch, 16) << 4) + digit(value.at(i + 1).vouch, 16)).toByte)

      data.immutable(using Unsafe)

extension (value: Text)
  def decode[SchemeType <: BinaryEncoding](using decodable: BinaryDecodable in SchemeType): Bytes = decodable.decode(value)

extension [ValueType: Digestible](value: ValueType)
  def digest[HashType <: HashScheme](using HashFunction of HashType): Digest of HashType =
    val digester = Digester(ValueType.digest(_, value))
    digester.apply

extension [ValueType: ByteCodec](value: ValueType)
  def hmac[HashType <: HashScheme](key: Bytes)(using function: HashFunction of HashType): Hmac of HashType =
    val mac = function.hmac0
    mac.init(SecretKeySpec(key.to(Array), function.name.s))

    Hmac:
      unsafely(mac.doFinal(ValueType.encode(value).mutable).nn.immutable)

extension (bytes: Bytes)
  def encodeAs[SchemeType <: BinaryEncoding](using encodable: BinaryEncodable in SchemeType): Text =
    encodable.encode(bytes)

extension [SourceType: Readable by Bytes](source: SourceType)
  def checksum[HashType <: HashScheme](using HashFunction of HashType): Digest of HashType =
    source.stream[Bytes].digest[HashType]
