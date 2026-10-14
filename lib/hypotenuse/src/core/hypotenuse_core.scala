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
package hypotenuse

import java.lang.{Integer as JInt, Long as JLong, Double as JDouble, Float as JFloat}

import scala.annotation.*
import scala.language.experimental.genericNumberLiterals
import scala.language.experimental.into
import scala.math

import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.{m, panic}
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*

export hypotenuse.internal.{B8, B16, B32, B64, S8, S16, S32, S64, U8, U16, U32, U64, F32, F64}

extension (inline context: StringContext)
  transparent inline def bin(): AnyVal = ${hypotenuse.protointernal.bin('context)}
  transparent inline def hex(): Array[Byte]^{} = ${hypotenuse.protointernal.hex('context)}

// The `Traversable` siblings of the `Iterable` extremum operations below, so the opaque
// collections answer to the same names; in this package (not `rudiments`) so the two receiver
// families stay overloaded rather than split-name-clashing at the `soundness` umbrella.
extension [self](self: self)(using traversable: self is Traversable)
  inline def minimum
    ( using commensurable: traversable.Operand is Commensurable against traversable.Operand )
  :   Optional[traversable.Operand] =

    val iterator = traversable.traverse(self)

    if !iterator.hasNext then Unset else
      var current = iterator.next()

      while iterator.hasNext do
        val element = iterator.next()
        if element < current then current = element

      current

  inline def maximum
    ( using commensurable: traversable.Operand is Commensurable against traversable.Operand )
  :   Optional[traversable.Operand] =

    val iterator = traversable.traverse(self)

    if !iterator.hasNext then Unset else
      var current = iterator.next()

      while iterator.hasNext do
        val element = iterator.next()
        if element > current then current = element

      current

  inline def minimize[key](lambda: traversable.Operand => key)
    ( using commensurable: key is Commensurable against key )
  :   Optional[traversable.Operand] =

    val iterator = traversable.traverse(self)

    if !iterator.hasNext then Unset else
      var current = iterator.next()
      var best = lambda(current)

      while iterator.hasNext do
        val element = iterator.next()
        val next = lambda(element)

        if next < best then
          current = element
          best = next

      current

  inline def maximize[key](lambda: traversable.Operand => key)
    ( using commensurable: key is Commensurable against key )
  :   Optional[traversable.Operand] =

    val iterator = traversable.traverse(self)

    if !iterator.hasNext then Unset else
      var current = iterator.next()
      var best = lambda(current)

      while iterator.hasNext do
        val element = iterator.next()
        val next = lambda(element)

        if next > best then
          current = element
          best = next

      current

extension [value](iterable: Iterable[value])
  inline def minimum(using commensurable: value is Commensurable against value): Optional[value] =
    if iterable.nil then Unset else
      var current = iterable.head

      iterable.tail.each: element => if element < current then current = element

      current

  inline def minimize[value2](lambda: value => value2)
    ( using commensurable: value2 is Commensurable against value2 )
  :   Optional[value] =

    if iterable.nil then Unset else
      var current = iterable.head
      var best = lambda(current)

      iterable.tail.each: element =>
        val next = lambda(element)

        if next < best then
          current = element
          best = next

      current

  inline def maximum(using commensurable: value is Commensurable against value): Optional[value] =
    if iterable.nil then Unset else
      var current = iterable.head

      iterable.tail.each: element => if element > current then current = element

      current

  inline def maximize[value2](lambda: value => value2)
    ( using commensurable: value2 is Commensurable against value2 )
  :   Optional[value] =

    if iterable.nil then Unset else
      var current = iterable.head
      var best = lambda(current)

      iterable.tail.each: element =>
        val next = lambda(element)

        if next > best then
          current = element
          best = next

      current

  inline def median
    ( using commensurable: value is Commensurable against value,
            subtractable:  value is Subtractable by value,
            divisible:     subtractable.Result is Divisible by Double,
            addable:       value is Addable by divisible.Result )
  :   Optional[addable.Result] =

    def recur(n: Int, items: List[value]): value = items match
      case pivot :: rest =>
        var left: List[value] = Nil
        var right: List[value] = Nil

        rest.each: item => if item < pivot then left ::= item else right ::= item

        if left.size == n then pivot
        else if left.size < n then recur(n - left.size - 1, right)
        else recur(n, left)

      case _ =>
        // Unreachable: `recur` is only ever applied to a list longer than `n >= 0`.
        panic(m"median of an empty list")

    val size = iterable.size

    if size == 0 then Unset
    else if size%2 == 0 then
      val first = recur((size - 1)/2, iterable.to(List))
      val second = recur((size + 1)/2, iterable.to(List))
      first + (second - first)/2.0
    else
      val result: value = recur((size - 1)/2, iterable.to(List))
      result + (result - result)/1.0


extension (float: Float)
  @targetName("absFloat")
  inline def abs: Float = math.abs(float)

  @targetName("ceilingFloat")
  inline def ceiling: Float = math.ceil(float).toFloat

  @targetName("floorFloat")
  inline def floor: Float = math.floor(float).toFloat

  @targetName("exponentFloat")
  inline def exponent: Int = math.getExponent(float)

  @targetName("incrementFloat")
  inline def increment: Float = math.nextUp(float)

  @targetName("decrementFloat")
  inline def decrement: Float = math.nextDown(float)

  @targetName("roundFloat")
  inline def round: Long = math.round(float)

  @targetName("scalbFloat")
  inline def scalb(scale: Int): Float = math.scalb(float, scale)

  @targetName("signumFloat")
  inline def signum: -1.0F | 0.0F | 1.0F = math.signum(float).asInstanceOf[-1.0F | 0.0F | 1.0F]

  @targetName("ulpFloat")
  inline def ulp: Float = math.ulp(float)

  @targetName("bitsFloat")
  inline def bits: Long = JFloat.floatToIntBits(float)

  @targetName("rawBitsFloat")
  inline def rawBits: Long = JFloat.floatToRawIntBits(float)

  @targetName("finiteFloat")
  inline def finite: Boolean = java.lang.Float.isFinite(float)

  @targetName("infiniteFloat")
  inline def infinite: Boolean = java.lang.Float.isInfinite(float)

  @targetName("nanFloat")
  inline def nan: Boolean = java.lang.Float.isNaN(float)

  @targetName("successorFloat")
  inline def successor: Float = Math.nextUp(float)

  @targetName("predecessorFloat")
  inline def predecessor: Float = Math.nextDown(float)

  @targetName("powerFloat")
  inline infix def ** (exponent: Double): Float = math.pow(float, exponent).toFloat

  @targetName("floorModFloat")
  inline infix def %% (right: Float): Float =
    val remainder: Float = float%right

    if remainder != 0.0f && (remainder > 0.0f) != (right > 0.0f) then remainder + right
    else remainder

extension (double: Double)
  @targetName("mantissaDouble")
  inline def mantissa: B64 = bits & 0xfffffffffffffL.bits

  @targetName("exponentDouble")
  inline def exponent: B16 = math.getExponent(double).toShort.bits

  @targetName("absDouble")
  inline def abs: Double = math.abs(double)

  @targetName("ceilingDouble")
  inline def ceiling: Double = math.ceil(double)

  @targetName("floorDouble")
  inline def floor: Double = math.floor(double)

  @targetName("incrementDouble")
  inline def increment: Double = math.nextUp(double)

  @targetName("decrementDouble")
  inline def decrement: Double = math.nextDown(double)

  @targetName("roundDouble")
  inline def round: Long = math.round(double)

  @targetName("scalbDouble")
  inline def scalb(scale: Int): Double = math.scalb(double, scale)

  @targetName("signumDouble")
  inline def signum: -1.0 | 0.0 | 1.0 = math.signum(double).asInstanceOf[-1.0 | 0.0 | 1.0]

  @targetName("ulpDouble")
  inline def ulp: Double = math.ulp(double)

  @targetName("bitsDouble")
  inline def bits: B64 = JDouble.doubleToLongBits(double).bits

  @targetName("rawBitsDouble")
  inline def rawBits: B64 = JDouble.doubleToRawLongBits(double).bits

  @targetName("finiteDouble")
  inline def finite: Boolean = java.lang.Double.isFinite(double)

  @targetName("infiniteDouble")
  inline def infinite: Boolean = java.lang.Double.isInfinite(double)

  @targetName("nanDouble")
  inline def nan: Boolean = java.lang.Double.isNaN(double)

  @targetName("successorDouble")
  inline def successor: Double = Math.nextUp(double)

  @targetName("predecessorDouble")
  inline def predecessor: Double = Math.nextDown(double)

  @targetName("powerDouble")
  inline infix def ** (exponent: Double): Double = math.pow(double, exponent)

  @targetName("floorModDouble")
  inline infix def %% (right: Double): Double =
    val remainder = double%right

    if remainder != 0.0 && (remainder > 0.0) != (right > 0.0) then remainder + right else remainder

extension (byte: Byte)
  @targetName("bitsByte")
  inline def bits: B8 = byte.asInstanceOf[B8]

  @targetName("longByte")
  inline def long: Long = byte.toLong

  @targetName("intByte")
  inline def int: Long = byte.toInt

  @targetName("shortByte")
  inline def short: Long = byte.toShort

  @targetName("absByte")
  inline def abs: Byte = math.abs(byte).toByte

  @targetName("powerByte")
  inline infix def ** (exponent: Double): Double = math.pow(byte.toDouble, exponent)

  @targetName("octalByte")
  inline def octal: Text = JInt.toOctalString(byte & 0xff).nn.tt

  @targetName("hexByte")
  inline def hex: Text = JInt.toHexString(byte & 0xff).nn.tt

  @targetName("base32Byte")
  inline def base32: Text = JInt.toString(byte, 32).tt

  @targetName("binaryByte")
  inline def binary: Text = JInt.toBinaryString(byte & 0xff).nn.tt

  @targetName("floorModByte")
  inline infix def %% (right: Int): Int = math.floorMod(byte, right)

  @targetName("floorDivByte")
  inline infix def /- (right: Int): Int = math.floorDiv(byte, right)

  @tailrec @targetName("gcdByte")
  def gcd(right: Byte): Byte = if right == 0 then byte else right.gcd((byte%right).toByte)

  @targetName("lcmByte")
  def lcm(right: Byte): Byte = (byte*right/byte.gcd(right)).toByte

extension (short: Short)
  @targetName("bitsShort")
  inline def bits: B16 = short.asInstanceOf[B16]

  @targetName("longShort")
  inline def long: Long = short.toLong

  @targetName("absShort")
  inline def abs: Short = math.abs(short).toShort

  @targetName("powerShort")
  inline infix def ** (exponent: Double): Double = math.pow(short.toDouble, exponent)

  @targetName("octalShort")
  inline def octal: Text = JInt.toOctalString(short & 0xffff).nn.tt

  @targetName("hexShort")
  inline def hex: Text = JInt.toHexString(short & 0xffff).nn.tt

  @targetName("base32Short")
  inline def base32: Text = JInt.toString(short, 32).tt

  @targetName("binaryShort")
  inline def binary: Text = JInt.toBinaryString(short & 0xffff).nn.tt

  @targetName("floorModShort")
  inline infix def %% (right: Short): Short = math.floorMod(short, right).toShort

  @targetName("floorDivShort")
  inline infix def /- (right: Short): Short = math.floorDiv(short, right).toShort

  @tailrec @targetName("gcdShort")
  def gcd(right: Short): Short = if right == 0 then short else right.gcd((short%right).toShort)

  @targetName("lcmShort")
  def lcm(right: Short): Short = (short*right/short.gcd(right)).toShort

extension (int: Int)
  @targetName("bitsInt")
  inline def bits: B32 = int.asInstanceOf[B32]

  @targetName("longInt")
  inline def long: Long = int.toLong

  @targetName("absInt")
  inline def abs: Int = math.abs(int)

  @targetName("powerInt")
  inline infix def ** (exponent: Double): Double = math.pow(int.toDouble, exponent)

  @targetName("octalInt")
  inline def octal: Text = JInt.toOctalString(int).nn.tt

  @targetName("hexInt")
  inline def hex: Text = JInt.toHexString(int).nn.tt

  @targetName("base32Int")
  inline def base32: Text = JInt.toString(int, 32).tt

  @targetName("binaryInt")
  inline def binary: Text = JInt.toBinaryString(int).nn.tt

  @targetName("floorModInt")
  inline infix def %% (right: Int): Int = math.floorMod(int, right)

  @targetName("floorDivInt")
  inline infix def /- (right: Int): Int = math.floorDiv(int, right)

  // `\` is in the highest precedence tier (every operator whose first character is none of
  // the special-cased ones), so it binds tighter than `*`, `/`, `+` and `-`: `1 \ 3 + 1 \ 6`
  // is the sum of two fractions, and needs no parentheses.
  @targetName("quotientInt")
  inline infix def \ (denominator: Int): Q64 = Q64(int.toLong, denominator.toLong)

  @tailrec @targetName("gcdInt")
  def gcd(right: Int): Int = if right == 0 then int else right.gcd(int%right)

  @targetName("lcmInt")
  def lcm(right: Int): Int = int*right/int.gcd(right)

extension (long: Long)
  @targetName("absLong")
  inline def abs: Long = math.abs(long)

  @targetName("bitsLong")
  inline def bits: B64 = long.asInstanceOf[B64]

  @targetName("octalLong")
  inline def octal: Text = JLong.toOctalString(long).nn.tt

  @targetName("hexLong")
  inline def hex: Text = JLong.toHexString(long).nn.tt

  @targetName("base32Long")
  inline def base32: Text = JLong.toString(long, 32).tt

  @targetName("binaryLong")
  inline def binary: Text = JLong.toBinaryString(long).nn.tt

  @targetName("floorModLong")
  inline infix def %% (right: Long): Long = math.floorMod(long, right)

  @targetName("floorDivLong")
  inline infix def /- (right: Long): Long = math.floorDiv(long, right)

  @targetName("quotientLong")
  inline infix def \ (denominator: Long): Q64 = Q64(long, denominator)

  @targetName("powerLong")
  inline infix def ** (exponent: Double): Double = math.pow(long.toDouble, exponent)

  @tailrec @targetName("gcdLong")
  def gcd(right: Long): Long = if right == 0 then long else right.gcd(long%right)

  @targetName("lcmLong")
  def lcm(right: Long): Long = long*right/long.gcd(right)

extension (doubleObject: Double.type)
  @unexported
  inline def apply(long: Long): Double = JDouble.longBitsToDouble(long)

extension (shortObject: Short.type)
  @unexported
  inline def apply(bits: B16): Short = bits.asInstanceOf[Short]

  @unexported
  def apply(bytes: Array[Byte]^{}): Short =
    (((bytes.readUnchecked(0) & 0xFF) << 8) | (bytes.readUnchecked(1) & 0xff)).toShort

extension (intObject: Int.type)
  @unexported
  inline def apply(bits: B32): Int = bits.asInstanceOf[Int]

  @unexported
  def apply(bytes: Array[Byte]^{}): Int =
    var int: Int = (bytes.readUnchecked(0) & 0xFF).toInt
    int <<= 8
    int |= (bytes.readUnchecked(1) & 0xFF).toInt
    int <<= 8
    int |= (bytes.readUnchecked(2) & 0xFF).toInt
    int <<= 8
    int |= (bytes.readUnchecked(3) & 0xFF).toInt

    int

extension (longObject: Long.type)
  @unexported
  inline def apply(bits: B64): Long = bits.asInstanceOf[Long]

  @unexported
  def apply(bytes: Array[Byte]^{}): Long =
    var long: Long = (bytes.readUnchecked(0) & 0xFF).toLong
    long <<= 8
    long |= (bytes.readUnchecked(1) & 0xFF).toLong
    long <<= 8
    long |= (bytes.readUnchecked(2) & 0xFF).toLong
    long <<= 8
    long |= (bytes.readUnchecked(3) & 0xFF).toLong
    long <<= 8
    long |= (bytes.readUnchecked(4) & 0xFF).toLong
    long <<= 8
    long |= (bytes.readUnchecked(5) & 0xFF).toLong
    long <<= 8
    long |= (bytes.readUnchecked(6) & 0xFF).toLong
    long <<= 8
    long |= (bytes.readUnchecked(7) & 0xFF).toLong

    long

def erf(value: Double): Double =
  val a = 0.254829592
  val b = -0.284496736
  val c = 1.421413741
  val d = -1.453152027
  val e = 1.061405429
  val p = 0.3275911

  val x = math.abs(value)
  val t = 1.0/(1.0 + p*x)
  val y = 1 - (((((e*t + d)*t) + c)*t + b)*t + a)*t*math.exp(-x*x)

  math.signum(value)*y

final val π = math.Pi
final val pi = math.Pi
final val euler = math.E
final val φ = (1.0 + 5.0.sqrt)/2.0
final val goldenRatio = φ

inline def cos(f64: Conversion.into[F64]): F64 = F64(math.cos(f64.double))
inline def acos(f64: Conversion.into[F64]): F64 = F64(math.acos(f64.double))
inline def cosh(f64: Conversion.into[F64]): F64 = F64(math.cosh(f64.double))
inline def sin(f64: Conversion.into[F64]): F64 = F64(math.sin(f64.double))
inline def asin(f64: Conversion.into[F64]): F64 = F64(math.asin(f64.double))
inline def sinh(f64: Conversion.into[F64]): F64 = F64(math.sinh(f64.double))
inline def tan(f64: Conversion.into[F64]): F64 = F64(math.tan(f64.double))
inline def atan(f64: Conversion.into[F64]): F64 = F64(math.atan(f64.double))

inline def hyp(first: Conversion.into[F64], second: F64): F64 =
  F64(math.hypot(first.double, second.double))

inline def exp(f64: Conversion.into[F64]): F64 = F64(math.exp(f64.double))
inline def expm1(f64: Conversion.into[F64]): F64 = F64(math.expm1(f64.double))
inline def ln(f64: Conversion.into[F64]): F64 = F64(math.log(f64.double))
inline def log10(f64: Conversion.into[F64]): F64 = F64(math.log10(f64.double))
inline def log1p(f64: Conversion.into[F64]): F64 = F64(math.log1p(f64.double))

extension [left](inline left: left)
  @targetName("lt")
  inline infix def < [right](inline right: right)
  ( using inline commensurable: left is Commensurable against right )
  :   Boolean =

    commensurable.compare(left, right, true, false)


  @targetName("lte")
  inline infix def <= [right](inline right: right)
    ( using inline commensurable: left is Commensurable against right )
  :   Boolean =

    commensurable.compare(left, right, false, false)


  @targetName("gt")
  inline infix def > [right](inline right: right)
    ( using inline commensurable: left is Commensurable against right )
  :   Boolean =

    commensurable.compare(left, right, true, true)


  @targetName("gte")
  inline infix def >= [right](inline right: right)
    ( using inline commensurable: left is Commensurable against right )
  :   Boolean =

    commensurable.compare(left, right, false, true)


  inline infix def min (inline right: left)(using left is Orderable): left =
    if left < right then left else right

  inline infix def max (inline right: left)(using left is Orderable): left =
    if left >= right then left else right

package arithmeticOptions:
  // Importing one of these lexically outranks the companion `divisible`, so `a/b` on
  // rationals yields a `Double` in that scope — which is what lets `std` (whose variance
  // must divide to `Double`) resolve for rational collections.
  given q64RationalDivision: Q64 is Divisible:
    type Operand = Q64
    type Result = Double

    def divide(dividend: Q64, divisor: Q64): Double = dividend.double/divisor.double

  given q32RationalDivision: Q32 is Divisible:
    type Operand = Q32
    type Result = Double

    def divide(dividend: Q32, divisor: Q32): Double = dividend.double/divisor.double

  // The policies are objects, and the givens name them by their singleton types, so that an
  // operator inlined under one of them refers to a pure path (dropped) rather than to an
  // instance (allocated). `CheckOverflow` explains.
  inline given uncheckedDivision: DivisionByZero.Unchecked.type = DivisionByZero.Unchecked
  inline given checkedDivision: DivisionByZero.Checked.type = DivisionByZero.Checked
  inline given uncheckedOverflow: CheckOverflow.Unchecked.type = CheckOverflow.Unchecked
  inline given checkedOverflow: CheckOverflow.Checked.type = CheckOverflow.Checked
