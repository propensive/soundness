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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.util.FromDigits

import anticipation.*
import contingency.*
import rudiments.*
import symbolism.*
import vacuous.*

// An arbitrary-precision decimal number: sign × magnitude × 10⁻ˢᶜᵃˡᵉ, as a single flat
// array — word 0 is the signum (-1, 0 or 1), word 1 the scale, and the remainder the
// magnitude in base-10⁹ limbs, least significant first. Base 10⁹ makes the decimal
// operations that define the type — scale alignment, rendering, trailing-zero stripping —
// digit-group shifts rather than long division, and keeps every intermediate product
// within a `Long`, so no wider arithmetic is needed on any platform.
//
// Every value is canonical: the magnitude has no high zero limb and no factor of ten (which
// is absorbed into the scale, as `stripTrailingZeros`), and zero is uniquely `[0, 0]`. So
// numerically-equal values are represented identically, and there is no counterpart of
// `BigDecimal`'s distinction between `equals` and `compareTo`.
// The opaque type and its operations are confined to `decimalInternal` — the pattern of
// `internal`'s numeric types — so the representation stays hidden even from the rest of
// this package, keeping companion-scope given resolution intact everywhere outside.
export decimalInternal.Decimal

object decimalInternal:
  opaque type Decimal = IArray[Int]

  object Decimal:
    private val Base: Int = 1000000000 // 10⁹, the largest power of ten below 2³¹
    private val BaseDigits: Int = 9

    private val powers: IArray[Int] =
      IArray(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, Base)

    val Zero: Decimal = IArray(0, 0)

    enum Rounding:
      case Up, Down, Ceiling, Floor, HalfUp, HalfDown, HalfEven

    def apply(value: Int): Decimal = apply(value.toLong)
    def apply(value: Long): Decimal = apply(value, 0)

    def apply(unscaled: Long, scale: Int): Decimal =
      if unscaled == 0L then Zero else
        val signum = if unscaled < 0 then -1 else 1
        var rest: Long = math.abs(unscaled)
        val magnitude = new Array[Int](3)
        var count = 0

        while rest != 0L do
          magnitude(count) = (rest%Base).toInt
          rest /= Base
          count += 1

        compose(signum, magnitude, count, scale)

    // A finite `Double` converts through its shortest round-tripping decimal representation
    // (`Double.toString`), so `Decimal(0.1)` is exactly `0.1` — not the 55-digit binary
    // expansion that `java.math.BigDecimal`'s `Double` constructor produces.
    def apply(value: Double): Decimal raises DecimalError =
      if value == 0.0 then Zero
      else if !java.lang.Double.isFinite(value) then abort(DecimalError(value.toString.tt))
      else parse(value.toString.tt)

    def parse(text: Text): Decimal raises DecimalError =
      parsed(text).or(abort(DecimalError(text)))

    // The grammar `[+|-] digits [. digits] [(e|E) [+|-] digits]`, with digits required on at
    // least one side of the point; `Unset` for anything else.
    private def parsed(text: Text): Optional[Decimal] =
      val input: String = text.s
      val length = input.length
      var index = 0
      var signum = 1

      if index < length && (input.charAt(index) == '+' || input.charAt(index) == '-') then
        if input.charAt(index) == '-' then signum = -1
        index += 1

      val digits = StringBuilder()
      var fraction = 0
      var point = false
      var bad = false

      while index < length && !bad
          && (input.charAt(index).isDigit || input.charAt(index) == '.') do
        if input.charAt(index) == '.' then
          if point then bad = true else point = true
        else
          digits.append(input.charAt(index))
          if point then fraction += 1

        index += 1

      if bad || digits.isEmpty then Unset else
        var exponent = 0L

        if index < length && (input.charAt(index) == 'e' || input.charAt(index) == 'E') then
          index += 1
          var exponentSign = 1

          if index < length && (input.charAt(index) == '+' || input.charAt(index) == '-') then
            if input.charAt(index) == '-' then exponentSign = -1
            index += 1

          if index >= length || !input.charAt(index).isDigit then bad = true

          while index < length && input.charAt(index).isDigit do
            exponent = math.min(exponent*10 + (input.charAt(index) - '0'), 2000000000L)
            index += 1

          exponent *= exponentSign

        if bad || index != length || math.abs(exponent) >= 2000000000L then Unset else
          // Skip leading zeros, then chunk the digits into base-10⁹ limbs from the right.
          var start = 0
          while start < digits.length - 1 && digits.charAt(start) == '0' do start += 1

          if digits.length - start == 1 && digits.charAt(start) == '0' then Zero else
            val significant = digits.length - start
            val count = (significant + BaseDigits - 1)/BaseDigits
            val magnitude = new Array[Int](count)
            var limb = 0
            var position = digits.length

            while position > start do
              val from = math.max(start, position - BaseDigits)
              var value = 0
              var i = from

              while i < position do
                value = value*10 + (digits.charAt(i) - '0')
                i += 1

              magnitude(limb) = value
              limb += 1
              position = from

            compose(signum, magnitude, count, fraction - exponent.toInt)

    // Builds the canonical form: high zero limbs dropped, factors of ten moved into the
    // scale (as `stripTrailingZeros`), and the unique zero when the magnitude vanishes. The
    // input array is clobbered, so callers always pass a fresh one.
    private[hypotenuse] def compose(signum: Int, magnitude: Array[Int], count0: Int, scale0: Int): Decimal =
      var count = count0
      while count > 0 && magnitude(count - 1) == 0 do count -= 1

      if count == 0 then Zero else
        var scale = scale0

        // A base-10⁹ number is divisible by ten exactly when its lowest limb is; whole zero
        // limbs strip nine digits at a time.
        while count > 1 && magnitude(0) == 0 do
          System.arraycopy(magnitude, 1, magnitude, 0, count - 1)
          count -= 1
          scale -= BaseDigits

        while magnitude(0)%10 == 0 && magnitude(0) != 0 || count > 1 && magnitude(0) == 0 do
          divideSmall(magnitude, count, 10)
          if count > 1 && magnitude(count - 1) == 0 then count -= 1
          scale -= 1

        val result = new Array[Int](count + 2)
        result(0) = signum
        result(1) = scale
        System.arraycopy(magnitude, 0, result, 2, count)
        result.immutable(using Unsafe)

    // In-place small division, returning the remainder; `divisor` is at most 10⁹.
    private def divideSmall(magnitude: Array[Int], count: Int, divisor: Int): Int =
      var remainder = 0L
      var i = count - 1

      while i >= 0 do
        val partial = remainder*Base + magnitude(i)
        magnitude(i) = (partial/divisor).toInt
        remainder = partial%divisor
        i -= 1

      remainder.toInt

    // The magnitude scaled up by 10ᵖᵒʷᵉʳ: whole limbs are prepended for each factor of 10⁹,
    // then a single small multiplication handles the residue.
    private[hypotenuse] def scaleUp(magnitude: Array[Int], count: Int, power: Int): (Array[Int], Int) =
      val shift = power/BaseDigits
      val residue = power%BaseDigits
      val result = new Array[Int](count + shift + 1)
      System.arraycopy(magnitude, 0, result, shift, count)
      var length = count + shift

      if residue > 0 then
        val factor = powers(residue)
        var carry = 0L
        var i = shift

        while i < length do
          val product = result(i).toLong*factor + carry
          result(i) = (product%Base).toInt
          carry = product/Base
          i += 1

        if carry != 0L then
          result(length) = carry.toInt
          length += 1

      (result, length)

    private[hypotenuse] def compareMagnitude
        (left: Array[Int], leftCount: Int, right: Array[Int], rightCount: Int): Int =
      if leftCount != rightCount then if leftCount < rightCount then -1 else 1 else
        var i = leftCount - 1
        var result = 0

        while i >= 0 && result == 0 do
          if left(i) != right(i) then result = if left(i) < right(i) then -1 else 1
          i -= 1

        result

    private[hypotenuse] def addMagnitude
        (left: Array[Int], leftCount: Int, right: Array[Int], rightCount: Int)
    :   (Array[Int], Int) =

      val count = math.max(leftCount, rightCount)
      val result = new Array[Int](count + 1)
      var carry = 0
      var i = 0

      while i < count do
        val sum = (if i < leftCount then left(i) else 0).toLong
            + (if i < rightCount then right(i) else 0) + carry

        if sum >= Base then
          result(i) = (sum - Base).toInt
          carry = 1
        else
          result(i) = sum.toInt
          carry = 0

        i += 1

      if carry == 1 then
        result(count) = 1
        (result, count + 1)
      else (result, count)

    // Subtraction of a smaller-or-equal magnitude from a larger.
    private[hypotenuse] def subtractMagnitude
        (left: Array[Int], leftCount: Int, right: Array[Int], rightCount: Int)
    :   (Array[Int], Int) =

      val result = new Array[Int](leftCount)
      var borrow = 0
      var i = 0

      while i < leftCount do
        val difference = left(i).toLong - (if i < rightCount then right(i) else 0) - borrow

        if difference < 0 then
          result(i) = (difference + Base).toInt
          borrow = 1
        else
          result(i) = difference.toInt
          borrow = 0

        i += 1

      var count = leftCount
      while count > 1 && result(count - 1) == 0 do count -= 1
      (result, count)

    private[hypotenuse] def multiplyMagnitude
        (left: Array[Int], leftCount: Int, right: Array[Int], rightCount: Int)
    :   (Array[Int], Int) =

      val result = new Array[Int](leftCount + rightCount)
      var i = 0

      while i < leftCount do
        var carry = 0L
        var j = 0

        while j < rightCount do
          val product = left(i).toLong*right(j) + result(i + j) + carry
          result(i + j) = (product%Base).toInt
          carry = product/Base
          j += 1

        var k = i + rightCount

        while carry != 0L do
          val sum = result(k) + carry
          result(k) = (sum%Base).toInt
          carry = sum/Base
          k += 1

        i += 1

      var count = leftCount + rightCount
      while count > 1 && result(count - 1) == 0 do count -= 1
      (result, count)

    // Knuth's algorithm D in base 10⁹: every trial numerator and product fits in a `Long`,
    // since 10¹⁸ < 2⁶³. Returns the quotient and remainder magnitudes.
    private[hypotenuse] def divideMagnitude
        (dividend: Array[Int], dividendCount: Int, divisor: Array[Int], divisorCount: Int)
    :   (Array[Int], Int, Array[Int], Int) =

      if divisorCount == 1 then
        val quotient = new Array[Int](dividendCount)
        System.arraycopy(dividend, 0, quotient, 0, dividendCount)
        val remainder = divideSmall(quotient, dividendCount, divisor(0))
        var count = dividendCount
        while count > 1 && quotient(count - 1) == 0 do count -= 1
        (quotient, count, Array(remainder), 1)
      else if compareMagnitude(dividend, dividendCount, divisor, divisorCount) < 0 then
        val remainder = new Array[Int](dividendCount)
        System.arraycopy(dividend, 0, remainder, 0, dividendCount)
        (Array(0), 1, remainder, dividendCount)
      else
        // Normalize so the divisor's top limb is at least half the base.
        val shift = Base/(divisor(divisorCount - 1) + 1)
        val u = new Array[Int](dividendCount + 1)
        val v = new Array[Int](divisorCount)

        var carry = 0L
        var i = 0

        while i < dividendCount do
          val product = dividend(i).toLong*shift + carry
          u(i) = (product%Base).toInt
          carry = product/Base
          i += 1

        u(dividendCount) = carry.toInt
        carry = 0L
        i = 0

        while i < divisorCount do
          val product = divisor(i).toLong*shift + carry
          v(i) = (product%Base).toInt
          carry = product/Base
          i += 1

        val quotientCount = dividendCount - divisorCount + 1
        val quotient = new Array[Int](quotientCount)
        val top = v(divisorCount - 1).toLong
        val second = v(divisorCount - 2).toLong
        var j = quotientCount - 1

        while j >= 0 do
          val numerator = u(j + divisorCount).toLong*Base + u(j + divisorCount - 1)
          var qhat = numerator/top
          var rhat = numerator%top
          var refining = true

          while refining && (qhat >= Base || qhat*second > rhat*Base + u(j + divisorCount - 2)) do
            qhat -= 1
            rhat += top
            if rhat >= Base then refining = false

          // Multiply-and-subtract; a rare overshoot of one is corrected by adding back.
          var borrow = 0L
          var k = 0

          while k < divisorCount do
            val product = qhat*v(k) + borrow
            val difference = u(j + k).toLong - product%Base

            if difference < 0 then
              u(j + k) = (difference + Base).toInt
              borrow = product/Base + 1
            else
              u(j + k) = difference.toInt
              borrow = product/Base

            k += 1

          val difference = u(j + divisorCount).toLong - borrow

          if difference < 0 then
            u(j + divisorCount) = (difference + Base).toInt
            qhat -= 1
            var carry2 = 0L
            k = 0

            while k < divisorCount do
              val sum = u(j + k).toLong + v(k) + carry2
              u(j + k) = (sum%Base).toInt
              carry2 = sum/Base
              k += 1

            u(j + divisorCount) = ((u(j + divisorCount) + carry2)%Base).toInt
          else u(j + divisorCount) = difference.toInt

          quotient(j) = qhat.toInt
          j -= 1

        var count = quotientCount
        while count > 1 && quotient(count - 1) == 0 do count -= 1
        divideSmall(u, divisorCount, shift)
        var remainderCount = divisorCount
        while remainderCount > 1 && u(remainderCount - 1) == 0 do remainderCount -= 1

        (quotient, count, u, remainderCount)

    private[hypotenuse] def magnitudeOf(decimal: Decimal): Array[Int] =
      val array = new Array[Int](decimal.length - 2)
      var i = 0

      while i < array.length do
        array(i) = decimal(i + 2)
        i += 1

      array

    private def digitLength(magnitude: Array[Int], count: Int): Int =
      var digits = (count - 1)*BaseDigits
      var topLimb = magnitude(count - 1)

      while topLimb > 0 do
        digits += 1
        topLimb /= 10

      digits

    // The signed three-way comparison behind `Orderable`.
    def comparison(left: Decimal, right: Decimal): Int =
      val leftSign = left(0)
      val rightSign = right(0)

      if leftSign != rightSign then if leftSign < rightSign then -1 else 1
      else if leftSign == 0 then 0
      else
        val leftMagnitude = magnitudeOf(left)
        val rightMagnitude = magnitudeOf(right)

        // The count of digits before the decimal point decides all but same-width cases,
        // avoiding alignment across pathological scale differences.
        val leftIntegral = digitLength(leftMagnitude, leftMagnitude.length) - left(1)
        val rightIntegral = digitLength(rightMagnitude, rightMagnitude.length) - right(1)

        val result =
          if leftIntegral != rightIntegral then if leftIntegral < rightIntegral then -1 else 1
          else
            val scale = math.max(left(1), right(1))

            val (leftAligned, leftCount) =
              if left(1) < scale
              then scaleUp(leftMagnitude, leftMagnitude.length, scale - left(1))
              else (leftMagnitude, leftMagnitude.length)

            val (rightAligned, rightCount) =
              if right(1) < scale
              then scaleUp(rightMagnitude, rightMagnitude.length, scale - right(1))
              else (rightMagnitude, rightMagnitude.length)

            compareMagnitude(leftAligned, leftCount, rightAligned, rightCount)

        if leftSign > 0 then result else -result

    // Aligns two nonzero operands to their common (larger) scale.
    private[hypotenuse] def aligned(left: Decimal, right: Decimal): (Array[Int], Int, Array[Int], Int, Int) =
      val scale = math.max(left(1), right(1))
      val leftMagnitude = magnitudeOf(left)
      val rightMagnitude = magnitudeOf(right)

      val (leftAligned, leftCount) =
        if left(1) < scale then scaleUp(leftMagnitude, leftMagnitude.length, scale - left(1))
        else (leftMagnitude, leftMagnitude.length)

      val (rightAligned, rightCount) =
        if right(1) < scale then scaleUp(rightMagnitude, rightMagnitude.length, scale - right(1))
        else (rightMagnitude, rightMagnitude.length)

      (leftAligned, leftCount, rightAligned, rightCount, scale)

    given orderable: Decimal is Orderable:
      inline def compare
          (inline left: Decimal, inline right: Decimal, inline strict: Boolean,
           inline greater: Boolean): Boolean =
        val result = comparison(left, right)

        if greater then (if strict then result > 0 else result >= 0)
        else (if strict then result < 0 else result <= 0)

    // `val price: Decimal = 12.99` under `genericNumberLiterals`. Literal digits are
    // compiler-validated, so parsing cannot fail for them.
    given fromDigits: FromDigits.Decimal[Decimal] = digits =>
      parsed(digits.tt).or(throw FromDigits.MalformedNumber(digits))

    def negation(value: Decimal): Decimal =
      if value(0) == 0 then value else
        val result = new Array[Int](value.length)
        var i = 0

        while i < value.length do
          result(i) = value(i)
          i += 1

        result(0) = -result(0)
        result.immutable(using Unsafe)

    def sum(left: Decimal, right: Decimal): Decimal =
      if left(0) == 0 then right else if right(0) == 0 then left else
        val (leftAligned, leftCount, rightAligned, rightCount, scale) = aligned(left, right)

        if left(0) == right(0) then
          val (total, count) = addMagnitude(leftAligned, leftCount, rightAligned, rightCount)
          compose(left(0), total, count, scale)
        else compareMagnitude(leftAligned, leftCount, rightAligned, rightCount) match
          case 0 =>
            Zero

          case order if order > 0 =>
            val (difference, count) =
              subtractMagnitude(leftAligned, leftCount, rightAligned, rightCount)

            compose(left(0), difference, count, scale)

          case _ =>
            val (difference, count) =
              subtractMagnitude(rightAligned, rightCount, leftAligned, leftCount)

            compose(right(0), difference, count, scale)

    def product(left: Decimal, right: Decimal): Decimal =
      if left(0) == 0 || right(0) == 0 then Zero else
        val leftMagnitude = magnitudeOf(left)
        val rightMagnitude = magnitudeOf(right)

        val (result, count) = multiplyMagnitude
          (leftMagnitude, leftMagnitude.length, rightMagnitude, rightMagnitude.length)

        compose(left(0)*right(0), result, count, left(1) + right(1))

    // The standard arithmetic operators come from symbolism, so `+`, `-`, `*` and prefix
    // negation resolve through the generic operators without any Decimal-specific exports.
    given addable: Decimal is Addable:
      type Operand = Decimal
      type Result = Decimal

      def add(augend: Decimal, addend: Decimal): Decimal = sum(augend, addend)

    given subtractable: Decimal is Subtractable:
      type Operand = Decimal
      type Result = Decimal

      def subtract(minuend: Decimal, subtrahend: Decimal): Decimal =
        sum(minuend, negation(subtrahend))

    given multiplicable: Decimal is Multiplicable:
      type Operand = Decimal
      type Result = Decimal

      def multiply(multiplicand: Decimal, multiplier: Decimal): Decimal =
        product(multiplicand, multiplier)

    given negatable: Decimal is Negatable:
      type Result = Decimal

      def negate(operand: Decimal): Decimal = negation(operand)

    given textualizable: Decimal is Textualizable = value => text(value)

    extension (left: Decimal)
      def signum: Int = left(0)
      def scale: Int = left(1)
      def abs: Decimal = if left(0) < 0 then Decimal.negation(left) else left

      // Division to an explicit result scale and rounding policy — the quotient of two
      // decimals generally does not terminate, so both must be chosen by the caller.
      def divide(right: Decimal, scale: Int, rounding: Decimal.Rounding)
      :   Decimal raises DivisionError =

        if right(0) == 0 then abort(DivisionError())
        else if left(0) == 0 then Decimal.Zero
        else
          val sign = left(0)*right(0)
          val power = scale + right(1) - left(1)
          val leftMagnitude = Decimal.magnitudeOf(left)
          val rightMagnitude = Decimal.magnitudeOf(right)

          val (dividend, dividendCount) =
            if power >= 0 then Decimal.scaleUp(leftMagnitude, leftMagnitude.length, power)
            else (leftMagnitude, leftMagnitude.length)

          val (divisor, divisorCount) =
            if power < 0 then Decimal.scaleUp(rightMagnitude, rightMagnitude.length, -power)
            else (rightMagnitude, rightMagnitude.length)

          val (quotient, quotientCount, remainder, remainderCount) =
            Decimal.divideMagnitude(dividend, dividendCount, divisor, divisorCount)

          val exact = remainderCount == 1 && remainder(0) == 0

          val increment: Boolean =
            if exact then false else rounding match
              case Decimal.Rounding.Down    => false
              case Decimal.Rounding.Up      => true
              case Decimal.Rounding.Ceiling => sign > 0
              case Decimal.Rounding.Floor   => sign < 0

              case half =>
                val (doubled, doubledCount) =
                  Decimal.addMagnitude(remainder, remainderCount, remainder, remainderCount)

                Decimal.compareMagnitude(doubled, doubledCount, divisor, divisorCount) match
                  case 0 => half match
                    case Decimal.Rounding.HalfUp   => true
                    case Decimal.Rounding.HalfDown => false
                    case _                         => quotient(0)%2 == 1

                  case order =>
                    order > 0

          val (rounded, roundedCount) =
            if increment then Decimal.addMagnitude(quotient, quotientCount, Array(1), 1)
            else (quotient, quotientCount)

          Decimal.compose(sign, rounded, roundedCount, scale)

      // Plain decimal notation — no exponent, no trailing zeros — with `BigDecimal`'s
      // `stripTrailingZeros.toPlainString` semantics built into the canonical form.
      def text: Text =
        if left(0) == 0 then "0".tt else
          val digits = StringBuilder()
          if left(0) < 0 then digits.append('-')
          val start = digits.length
          var i = left.length - 1
          digits.append(left(i).toString)
          i -= 1

          while i >= 2 do
            val group = left(i).toString
            var pad = 9 - group.length

            while pad > 0 do
              digits.append('0')
              pad -= 1

            digits.append(group)
            i -= 1

          val scale = left(1)
          val count = digits.length - start

          if scale <= 0 then
            var zeros = -scale

            while zeros > 0 do
              digits.append('0')
              zeros -= 1
          else if count > scale then digits.insert(digits.length - scale, '.')
          else
            val prefix = StringBuilder("0.")
            var zeros = scale - count
            while zeros > 0 do
              prefix.append('0')
              zeros -= 1
            digits.insert(start, prefix)

          digits.toString.tt

      // The nearest `Double`, by way of the decimal text — correctly rounded, since parsing
      // decimal text is itself correctly rounded.
      def double: Double = java.lang.Double.parseDouble(text.s)

      def whole: Boolean = left(1) <= 0
