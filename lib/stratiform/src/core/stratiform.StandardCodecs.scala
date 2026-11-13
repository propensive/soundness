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
package stratiform

import java.math.BigInteger

import scala.language.unsafeNulls

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*
import zephyrine.*

// The standard codecs of the Standard Codecs Specification: integers, IEEE 754 binary64, UUIDs
// and BLAKE3-256 digests. Each has one fixed width, and its bytes compare bytewise in the
// order of the values they denote, which is what lets RaTEL store a value inline in a slot and
// compare index keys with no knowledge of their type. The accepted texts are canonical: a text
// which denotes a value in range but is not that value's one rendering is rejected.
private[stratiform] object StandardCodecs:
  import Tel.Codec.{Decoded, Encoded}

  // The fixed width, in bytes, of the standard codec of a name (§9), if it is one.
  def width(name: Text): Optional[Int] = name.s match
    case "u8" | "i8"           => 1
    case "u16" | "i16"         => 2
    case "u32" | "i32"         => 4
    case "u64" | "i64" | "f64" => 8
    case "uuid"                => 16
    case "blake3-256"          => 32
    case _                     => Unset

  def apply(name: Text): Optional[Tel.Codec] = name.s match
    case "u8"         => u8
    case "u16"        => u16
    case "u32"        => u32
    case "u64"        => u64
    case "i8"         => i8
    case "i16"        => i16
    case "i32"        => i32
    case "i64"        => i64
    case "f64"        => f64
    case "uuid"       => uuid
    case "blake3-256" => blake3
    case _            => Unset

  val u8: Tel.Codec = new Whole(1, false)
  val u16: Tel.Codec = new Whole(2, false)
  val u32: Tel.Codec = new Whole(4, false)
  val u64: Tel.Codec = new Whole(8, false)
  val i8: Tel.Codec = new Whole(1, true)
  val i16: Tel.Codec = new Whole(2, true)
  val i32: Tel.Codec = new Whole(4, true)
  val i64: Tel.Codec = new Whole(8, true)

  // The low `width` bytes of a word, most significant first.
  private def bigEndian(word: Long, width: Int): Data =
    Array.tabulate[Byte](width): index =>
      (word >>> 8*(width - 1 - index)).toByte

  private def digit(char: Char): Boolean = char >= '0' && char <= '9'

  // `"0" | [1-9][0-9]*`: the one rendering of a natural number.
  private def natural(string: String): Boolean =
    string == "0" || !string.isEmpty && !string.startsWith("0") && string.forall(digit)

  // §4 and §5: an integer of `width` bytes, big-endian, a signed one offset by half its range
  // so that the negative values sort below the others.
  private class Whole(width: Int, signed: Boolean) extends Tel.Codec:
    private val bits: Int = 8*width
    private val modulus: BigInteger = BigInteger.ONE.shiftLeft(bits)

    private val bias: BigInteger =
      if signed then BigInteger.ONE.shiftLeft(bits - 1) else BigInteger.ZERO

    private val name: String = (if signed then "i" else "u")+bits

    def encode(text: Text): Encoded =
      val string = text.s
      val negative = signed && string.startsWith("-")
      val magnitude = if negative then string.substring(1) else string

      // Twenty digits hold every 64-bit value, and bound the work done on a hostile text.
      if !natural(magnitude) || magnitude.length > 20 || negative && magnitude == "0"
      then Encoded.Invalid(t"the text is not a canonical $name integer")
      else
        val value = BigInteger(string).add(bias)

        if value.signum < 0 || value.compareTo(modulus) >= 0
        then Encoded.Invalid(t"the integer is out of range for $name")
        else
          Encoded.Bytes(bigEndian(value.longValue, width))

    def decode(bytes: Data): Decoded =
      if bytes.length != width then Decoded.Failure(t"$name is $width bytes, not ${bytes.length}")
      else
        val value = bytes.fuse(BigInteger.ZERO)(state.shiftLeft(8).add(BigInteger.valueOf(next & 0xff)))
        Decoded.Value(value.subtract(bias).toString.tt)

  // §6: an IEEE 754 binary64, as its bit pattern with the sign bit inverted, and a negative
  // value's other bits inverted too, so that the bytes sort numerically.
  val f64: Tel.Codec = new Tel.Codec:
    private def pattern(char: Char): Boolean =
      digit(char) || char == '.' || char == '-' || char == '+' || char == 'e'

    def encode(text: Text): Encoded =
      val string = text.s

      val value: Optional[Double] = string match
        case "Infinity"  => Double.PositiveInfinity
        case "-Infinity" => Double.NegativeInfinity

        case _ =>
          // A cheap filter for what `parseDouble` would accept and the grammar does not
          // (`NaN`, hexadecimal, a type suffix); the canonical rendering is the real test.
          if string.isEmpty || string.length > 32 || !string.forall(pattern) then Unset
          else try java.lang.Double.parseDouble(string) catch case _: NumberFormatException => Unset

      value.let: value =>
        if render(value) != string then Unset else
          val bits = java.lang.Double.doubleToRawLongBits(value)
          val ordered = if bits < 0L then ~bits else bits ^ Long.MinValue
          Encoded.Bytes(bigEndian(ordered, 8)): Encoded

      . or(Encoded.Invalid(t"the text is not the canonical rendering of a binary64 value"))

    def decode(bytes: Data): Decoded =
      if bytes.length != 8 then Decoded.Failure(t"f64 is 8 bytes, not ${bytes.length}") else
        val ordered = bytes.u64be(0)
        val bits = if ordered < 0L then ordered ^ Long.MinValue else ~ordered
        val value = java.lang.Double.longBitsToDouble(bits)

        if value.isNaN then Decoded.Failure(t"the bytes are a NaN, which has no text")
        else Decoded.Value(render(value).tt)

  // §6.2: the canonical rendering of a value which is not a NaN. The shortest digits which
  // round-trip are taken from the platform, whose own layout of them is discarded: only the
  // significand `d₁…d_k` and the exponent `n`, such that the value is `0.d₁…d_k × 10ⁿ`, are
  // kept, and laid out by the rules of the specification.
  private[stratiform] def render(value: Double): String =
    if value == Double.PositiveInfinity then "Infinity"
    else if value == Double.NegativeInfinity then "-Infinity"
    else if value == 0.0 then (if 1.0/value < 0.0 then "-0" else "0")
    else if value < 0.0 then "-"+render(-value)
    else
      val platform = java.lang.Double.toString(value).toLowerCase
      val mark = platform.indexOf('e')
      val mantissa = if mark < 0 then platform else platform.substring(0, mark)
      val power = if mark < 0 then "0" else platform.substring(mark + 1).stripPrefix("+")
      val exponent = Integer.parseInt(power)
      val point = mantissa.indexOf('.')
      val whole = if point < 0 then mantissa else mantissa.substring(0, point)
      val fraction = if point < 0 then "" else mantissa.substring(point + 1)
      val all = whole+fraction
      val leading = all.indexWhere(_ != '0')
      val significand = all.substring(leading).reverse.dropWhile(_ == '0').reverse
      val (digits, n) = shortest(value, significand, whole.length - leading + exponent)
      val k = digits.length

      if k <= n && n <= 21 then digits+"0"*(n - k)
      else if 0 < n && n <= 21 then digits.substring(0, n)+"."+digits.substring(n)
      else if -6 < n && n <= 0 then "0."+"0"*(-n)+digits
      else
        val sign = if n >= 1 then "+" else "-"
        val tail = if k > 1 then "."+digits.substring(1) else ""
        digits.substring(0, 1)+tail+"e"+sign+(n - 1).abs

  // A platform may never render fewer than two digits (the JVM writes the least positive value
  // as `4.9E-324`, though `5e-324` denotes it too), so a pair of digits is not yet known to be
  // the shortest: if a single digit round-trips, it is, and of two which do, the closer to
  // the value is chosen, and of two equally close, the even one. Rounding `9` up carries into
  // the exponent.
  private def shortest(value: Double, digits: String, n: Int): (String, Int) =
    if digits.length != 2 then (digits, n) else
      val exact = java.math.BigDecimal(value)
      val low = Integer.parseInt(digits.substring(0, 1))

      def candidate(digit: Int): Optional[(java.math.BigDecimal, (String, Int))] =
        val single: (String, Int) = if digit == 10 then ("1", n + 1) else (digit.toString, n)
        val text = single(0)+"e"+(single(1) - 1)

        if java.lang.Double.parseDouble(text) != value then Unset
        else (java.math.BigDecimal(text).subtract(exact).abs, single)

      candidate(low).lay(candidate(low + 1).lay((digits, n))(_(1))): lower =>
        candidate(low + 1).lay(lower(1)): upper =>
          val order = lower(0).compareTo(upper(0))
          if order < 0 || order == 0 && low%2 == 0 then lower(1) else upper(1)

  private def hexadecimal(char: Char): Boolean = digit(char) || char >= 'a' && char <= 'f'

  // Lowercase hexadecimal digits, with a hyphen before each of the digit positions in
  // `hyphens`, as `count` bytes in the order they are written.
  private class Hexadecimal(name: String, count: Int, hyphens: Int => Boolean) extends Tel.Codec:
    def encode(text: Text): Encoded =
      val string = text.s
      val plain = string.filter(_ != '-')

      // The digits as they are written, a hyphen before each position which takes one.
      def placed: String =
        plain.indices.map: index =>
          (if hyphens(index) then "-" else "")+plain.substring(index, index + 1)
        . mkString

      if plain.length != 2*count || !plain.forall(hexadecimal) || placed != string
      then Encoded.Invalid(t"the text is not a canonical $name")
      else Encoded.Bytes:
        Array.tabulate[Byte](count): index =>
          Integer.parseInt(plain.substring(2*index, 2*index + 2), 16).toByte

    def decode(bytes: Data): Decoded =
      if bytes.length != count
      then Decoded.Failure(t"$name is $count bytes, not ${bytes.length}")
      else
        val builder = StringBuilder()

        (0 until count).foreach: index =>
          if hyphens(2*index) then builder.append('-')
          val byte = bytes.u8(index)
          builder.append(Integer.toHexString(byte | 0x100).substring(1))

        Decoded.Value(builder.toString.tt)

  // §7: the RFC 9562 hyphenated form, in lowercase, as its sixteen bytes.
  val uuid: Tel.Codec =
    new Hexadecimal("uuid", 16, index => index >= 8 && index <= 20 && index%4 == 0)

  // §8: sixty-four lowercase hexadecimal digits, as their thirty-two bytes.
  val blake3: Tel.Codec = new Hexadecimal("blake3-256", 32, _ => false)
