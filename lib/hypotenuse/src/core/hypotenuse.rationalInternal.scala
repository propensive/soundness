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

import scala.caps
import scala.math
import scala.reflect.ClassTag
import scala.util.FromDigits

import java.lang.{Double as JDouble, Long as JLong}

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*

// Exact rational numbers in a single machine word: `Q64` over `Long` and `Q32` over `Int`.
//
// The magnitude is a position in the Stern–Brocot tree, stored as the value's canonical
// continued fraction [a₀; a₁, …, aₖ] (with aₖ ≥ 2): a sentinel 1 bit, then the Elias-gamma
// codes of a₀+1, a₁, …, aₖ, most significant term first. The top bit of the word is the
// sign; an all-zero magnitude is zero when positive, and NaR — "not a rational", the result
// of division by zero — when negative (`Long.MinValue`/`Int.MinValue`). NaR propagates
// through arithmetic and compares as unordered, like `Double.NaN`.
//
// Gamma-coding the terms (rather than storing the tree path's steps unary) makes the cost
// of a term logarithmic in its size: with the 62 payload bits of `Q64`, integers reach
// 2³¹−2 and unit fractions 1/(2³¹−1), while the deepest all-ones chains bound every decoded
// numerator and denominator below 2⁴⁴. `Q32`'s 30 payload bits reach ±32766 and 1/32767,
// with numerators and denominators below 2²¹. Every rational whose numerator and
// denominator are at most 2²⁴ (Q64) or 2¹¹ (Q32) is exactly representable.
//
// Every value is canonical: each rational has exactly one encoding, already in lowest
// terms, so bit-equality is value-equality and no gcd normalization exists anywhere. When
// an exact result needs more than the payload budget, the encoder keeps the longest prefix
// of its continued fraction that fits, reducing the final term where possible — the result
// is a convergent or semiconvergent of the exact value, the best rational approximation
// among all rationals of no greater denominator (at term granularity; not global
// round-to-nearest).
//
// The opaque types and their operations are confined to `rationalInternal` — the pattern of
// `internal`'s numeric types — so the representation stays hidden even from the rest of
// this package, keeping companion-scope given resolution intact everywhere outside.
export rationalInternal.{Q32, Q64}

object rationalInternal:
  // The `caps.Pure` bounds make purity visible outside the opaque scope, so collections of
  // rationals never box their elements with a fresh capture set (the `ultimatum.Fraction`
  // pattern).
  opaque type Q64 <: Matchable & caps.Pure = Long & caps.Pure
  opaque type Q32 <: Matchable & caps.Pure = Int & caps.Pure

  // `caps.Pure` is an erased marker, so each type still erases to its representation; the
  // casts are runtime no-ops that let the capture checker treat a rational as pure.
  private inline def q64(value: Long): Q64 = value.asInstanceOf[Q64]
  private inline def q32(value: Int): Q32 = value.asInstanceOf[Q32]

  // Any continued-fraction term of at least 2³¹ overflows every payload budget, so a term
  // this size acts as a saturating sentinel: emitting it ends term generation, and the
  // encoder's truncation replaces it with the largest term that fits.
  private val Cap: Long = 0x80000000L

  private val Budget64: Int = 62
  private val Budget32: Int = 30

  // Terms all cost at least one bit, so no encoding ever reads beyond the budget's worth of
  // terms; the generators below stop early once the budget is exceeded.
  private val TermLimit: Int = 96

  // The Elias-gamma code of b ≥ 1 is ⌊log₂ b⌋ zeros followed by the binary of b.
  private def gammaLength(term: Long): Int = 2*(63 - JLong.numberOfLeadingZeros(term)) + 1

  // The stored code for term i is the gamma code of a₀+1 at the head — a₀ may be zero for
  // values below one — and of aᵢ elsewhere.
  private def stored(terms: scala.Array[Long], index: Int): Long =
    if index == 0 then terms(0) + 1 else terms(index)

  // Packs continued-fraction terms into a magnitude word: a sentinel 1, then each term's
  // gamma code. When the terms overrun the budget, the final kept term is reduced to the
  // largest value that fits (a semiconvergent) or dropped (the previous convergent), and a
  // trailing term of 1 is merged upward — […, x, 1] = […, x+1] — to keep the encoding
  // canonical.
  private def encodeMagnitude(terms: scala.Array[Long]^, count0: Int, budget: Int): Long =
    var used = 0
    var fit = 0

    while fit < count0 && used + gammaLength(stored(terms, fit)) <= budget do
      used += gammaLength(stored(terms, fit))
      fit += 1

    var count = count0

    if fit < count0 then
      val remaining = budget - used
      val most = if remaining <= 0 then 0L else (1L << (remaining + 1)/2) - 1
      val reduced = math.min(stored(terms, fit) - 1, most)

      if reduced >= (if fit == 0 then 2L else 1L) then
        terms(fit) = if fit == 0 then reduced - 1 else reduced
        count = fit + 1
      else count = fit

    var settled = false

    while !settled do
      if count == 0 then return 0L
      else if count == 1 then
        if terms(0) == 0L then return 0L else settled = true
      else if terms(count - 1) >= 2L then
        var length = 0
        var i = 0

        while i < count do
          length += gammaLength(stored(terms, i))
          i += 1

        if length <= budget then settled = true else count -= 1
      else
        terms(count - 2) += 1
        count -= 1

    var accumulator = 1L
    var i = 0

    while i < count do
      val code = stored(terms, i)
      accumulator = (accumulator << gammaLength(code)) | code
      i += 1

    accumulator

  // Reads the gamma-coded terms back out of a nonzero magnitude, recovering the a-values.
  private def decodeTerms(magnitude: Long, terms: scala.Array[Long]^): Int =
    var position = 62 - JLong.numberOfLeadingZeros(magnitude)
    var count = 0

    while position >= 0 do
      var zeros = 0

      while (magnitude >> position & 1L) == 0L do
        zeros += 1
        position -= 1

      var value = 0L
      var i = zeros + 1

      while i > 0 do
        value = (value << 1) | (magnitude >> position & 1L)
        position -= 1
        i -= 1

      terms(count) = if count == 0 then value - 1 else value
      count += 1

    count

  // Folds continued-fraction terms into the numerator and denominator by the continuant
  // recurrence; both stay below 2⁴⁴ for any encodable term sequence.
  private def continuants(terms: scala.Array[Long], count: Int): (Long, Long) =
    var numerator = 1L
    var numerator0 = 0L
    var denominator = 0L
    var denominator0 = 1L
    var i = 0

    while i < count do
      val term = terms(i)
      val nextNumerator = term*numerator + numerator0
      val nextDenominator = term*denominator + denominator0
      numerator0 = numerator
      denominator0 = denominator
      numerator = nextNumerator
      denominator = nextDenominator
      i += 1

    (numerator, denominator)

  // The Euclidean algorithm, appending continued-fraction terms from `offset` and
  // saturating at `Cap`; the final term of an exact run is always at least 2, so the output
  // is canonical without adjustment.
  private def euclidean(numerator: Long, denominator: Long, terms: scala.Array[Long]^, offset: Int)
  :   Int =
    var high = numerator
    var low = denominator
    var count = offset

    while low != 0L && count < TermLimit do
      val quotient = high/low

      if quotient >= Cap then
        terms(count) = Cap
        count += 1
        low = 0L
      else
        terms(count) = quotient
        count += 1
        val remainder = high%low
        high = low
        low = remainder

    count

  private def encodeFraction(numerator: Long, denominator: Long, budget: Int): Long =
    if numerator == 0L then 0L else
      val terms = new scala.Array[Long](TermLimit)
      encodeMagnitude(terms, euclidean(numerator, denominator, terms, 0), budget)

  // Wide intermediates — cross-products of decoded Q64 fractions reach 2⁸⁸, and the square
  // comparisons in `sqrtMagnitude` 2¹³¹ — are fixed six-limb little-endian arrays in base
  // 2³¹, so every limb product fits comfortably in a `Long` on any platform.
  private val Limbs: Int = 6
  private val LimbMask: Long = 0x7fffffffL

  private def limbsClear(limbs: scala.Array[Long]^): Unit =
    var i = 0

    while i < Limbs do
      limbs(i) = 0L
      i += 1

  private def limbsFromLong(limbs: scala.Array[Long]^, value: Long): Unit =
    limbsClear(limbs)
    limbs(0) = value & LimbMask
    limbs(1) = (value >>> 31) & LimbMask
    limbs(2) = value >>> 62

  private def limbsToLong(limbs: scala.Array[Long]): Long =
    limbs(0) | (limbs(1) << 31) | (limbs(2) << 62)

  private def limbsBitLength(limbs: scala.Array[Long]): Int =
    var top = Limbs - 1
    while top >= 0 && limbs(top) == 0L do top -= 1
    if top < 0 then 0 else 31*top + (64 - JLong.numberOfLeadingZeros(limbs(top)))

  private def limbsCompare(left: scala.Array[Long], right: scala.Array[Long]): Int =
    var i = Limbs - 1
    var result = 0

    while i >= 0 && result == 0 do
      if left(i) != right(i) then result = if left(i) < right(i) then -1 else 1
      i -= 1

    result

  private def limbsAdd(into: scala.Array[Long]^, addend: scala.Array[Long]): Unit =
    var carry = 0L
    var i = 0

    while i < Limbs do
      val sum = into(i) + addend(i) + carry
      into(i) = sum & LimbMask
      carry = sum >>> 31
      i += 1

  // Subtraction of a smaller-or-equal value from a larger.
  private def limbsSubtract(into: scala.Array[Long]^, subtrahend: scala.Array[Long]): Unit =
    var borrow = 0L
    var i = 0

    while i < Limbs do
      val difference = into(i) - subtrahend(i) - borrow

      if difference < 0L then
        into(i) = difference + (1L << 31)
        borrow = 1L
      else
        into(i) = difference
        borrow = 0L

      i += 1

  private def limbsCopy(source: scala.Array[Long], target: scala.Array[Long]^): Unit =
    var i = 0

    while i < Limbs do
      target(i) = source(i)
      i += 1

  // The product of two nonnegative `Long`s, by 31-bit pieces; column accumulations stay far
  // below the `Long` range before the final carry pass.
  private def limbsMultiply(left: Long, right: Long, into: scala.Array[Long]^): Unit =
    limbsClear(into)
    var i = 0

    while i < 3 do
      val piece = i match
        case 0 => left & LimbMask
        case 1 => (left >>> 31) & LimbMask
        case _ => left >>> 62

      if piece != 0L then
        var j = 0

        while j < 3 do
          val other = j match
            case 0 => right & LimbMask
            case 1 => (right >>> 31) & LimbMask
            case _ => right >>> 62

          if other != 0L then
            val product = piece*other
            into(i + j) += product & LimbMask
            into(i + j + 1) += product >>> 31

          j += 1

      i += 1

    var carry = 0L
    var k = 0

    while k < Limbs do
      val sum = into(k) + carry
      into(k) = sum & LimbMask
      carry = sum >>> 31
      k += 1

  // A wide value times a nonnegative `Long`, for the square-times-operand comparisons in
  // `sqrtMagnitude`; the true product never exceeds the six-limb capacity there.
  private def limbsMultiplyByLong(limbs: scala.Array[Long], value: Long, into: scala.Array[Long]^)
  :   Unit =
    limbsClear(into)
    var j = 0

    while j < 3 do
      val piece = j match
        case 0 => value & LimbMask
        case 1 => (value >>> 31) & LimbMask
        case _ => value >>> 62

      if piece != 0L then
        var i = 0

        while i + j < Limbs do
          val product = limbs(i)*piece
          into(i + j) += product & LimbMask
          if i + j + 1 < Limbs then into(i + j + 1) += product >>> 31
          i += 1

      j += 1

    var carry = 0L
    var k = 0

    while k < Limbs do
      val sum = into(k) + carry
      into(k) = sum & LimbMask
      carry = sum >>> 31
      k += 1

  // Exact division of a wide value by a `Long` divisor whose quotient is known to fit in a
  // `Long`, as arises from the surd recurrence in `sqrtMagnitude`.
  private def limbsDivideByLong(limbs: scala.Array[Long], divisor: Long): Long =
    var remainder = 0L
    var quotient = 0L
    var i = Limbs - 1

    while i >= 0 do
      val current = (remainder << 31) | limbs(i)
      quotient = (quotient << 31) | (current/divisor)
      remainder = current%divisor
      i -= 1

    quotient

  // Restoring binary division of one wide value by another, leaving the remainder in
  // `remainder`. Quotients of 2³² or more saturate to `Cap`, which ends term generation, so
  // the returned quotient always fits easily in a `Long`.
  private def limbsDivide
    ( dividend:  scala.Array[Long],
      divisor:   scala.Array[Long],
      remainder: scala.Array[Long]^ )
  :   Long =

    val excess = limbsBitLength(dividend) - limbsBitLength(divisor)
    if excess >= 32 then Cap else
      limbsClear(remainder)
      var quotient = 0L
      var i = limbsBitLength(dividend) - 1

      while i >= 0 do
        var carry = (dividend(i/31) >> i%31) & 1L
        var k = 0

        while k < Limbs do
          val shifted = (remainder(k) << 1) | carry
          carry = shifted >>> 31
          remainder(k) = shifted & LimbMask
          k += 1

        quotient <<= 1

        if limbsCompare(remainder, divisor) >= 0 then
          limbsSubtract(remainder, divisor)
          quotient |= 1L

        i -= 1

      quotient

  // The Euclidean algorithm over wide values, switching to `Long` arithmetic as soon as
  // both operands fit; the common factors of an unreduced pair cancel in the remainders, so
  // no gcd reduction is needed first.
  private def euclideanWide
    ( numerator: scala.Array[Long]^, denominator: scala.Array[Long]^, terms: scala.Array[Long]^ )
  :   Int =

    val remainder = new scala.Array[Long](Limbs)
    var count = 0
    var finished = false

    while !finished && count < TermLimit do
      if limbsBitLength(denominator) == 0 then finished = true
      else if limbsBitLength(numerator) <= 62 && limbsBitLength(denominator) <= 62 then
        count = euclidean(limbsToLong(numerator), limbsToLong(denominator), terms, count)
        finished = true
      else
        val quotient = limbsDivide(numerator, denominator, remainder)

        if quotient >= Cap then
          terms(count) = Cap
          count += 1
          finished = true
        else
          terms(count) = quotient
          count += 1
          limbsCopy(denominator, numerator)
          limbsCopy(remainder, denominator)

    count

  // The floor of the square root of a wide value, by binary search; the root always fits
  // well within a `Long`.
  private def limbsSquareRoot(value: scala.Array[Long]): Long =
    val square = new scala.Array[Long](Limbs)
    var low = 0L
    var high = 1L << math.min(62, limbsBitLength(value)/2 + 1)

    while high - low > 1L do
      val middle = low + (high - low)/2
      limbsMultiply(middle, middle, square)
      if limbsCompare(square, value) <= 0 then low = middle else high = middle

    low

  // The magnitude word of √(n/d) for a canonical positive fraction: exact when nd is a
  // perfect square — which for coprime n and d is exactly when both are — and otherwise the
  // periodic surd recurrence for (P + √N)/Q generates terms until the budget truncates.
  private def sqrtMagnitude(numerator: Long, denominator: Long, budget: Int): Long =
    val wide = new scala.Array[Long](Limbs)
    limbsMultiply(numerator, denominator, wide)
    val root = limbsSquareRoot(wide)
    val square = new scala.Array[Long](Limbs)
    limbsMultiply(root, root, square)

    if limbsCompare(square, wide) == 0 then encodeFraction(root, denominator, budget) else
      val terms = new scala.Array[Long](TermLimit)
      val work = new scala.Array[Long](Limbs)
      var p = 0L
      var q = denominator
      var count = 0
      var length = 0
      var capped = false

      while !capped && length <= budget + 2 && count < TermLimit do
        var term = (p + root)/q
        val candidate = (term + 1L)*q - p

        if candidate <= root then term += 1L
        else if candidate == root + 1L then
          limbsMultiply(candidate, candidate, square)
          if limbsCompare(square, wide) < 0 then term += 1L

        if term >= Cap then
          terms(count) = Cap
          count += 1
          capped = true
        else
          terms(count) = term
          length += gammaLength(stored(terms, count))
          count += 1
          val p2 = term*q - p
          limbsMultiply(p2, p2, square)
          limbsCopy(wide, work)
          limbsSubtract(work, square)
          q = limbsDivideByLong(work, q)
          p = p2

      encodeMagnitude(terms, count, budget)

  // The best encodable approximation of a positive, finite `Double`: exactly m·2ᵉ, with the
  // power of two folded into whichever side of the fraction it belongs.
  private def doubleMagnitude(value: Double, budget: Int): Long =
    val bits = JDouble.doubleToLongBits(value)
    val rawExponent = (bits >>> 52 & 0x7ffL).toInt
    var mantissa = if rawExponent == 0 then bits & 0xfffffffffffffL
                   else bits & 0xfffffffffffffL | (1L << 52)
    var exponent = if rawExponent == 0 then -1074 else rawExponent - 1075
    val shift = JLong.numberOfTrailingZeros(mantissa)
    mantissa >>>= shift
    exponent += shift
    val width = 64 - JLong.numberOfLeadingZeros(mantissa)
    val terms = new scala.Array[Long](TermLimit)

    if exponent >= 0 then
      if width + exponent > 32 then
        terms(0) = Cap
        encodeMagnitude(terms, 1, budget)
      else encodeFraction(mantissa << exponent, 1L, budget)
    else if -exponent <= 62 then encodeFraction(mantissa, 1L << -exponent, budget)
    else if -exponent - width >= 32 then
      terms(0) = 0L
      terms(1) = Cap
      encodeMagnitude(terms, 2, budget)
    else
      val wideNumerator = new scala.Array[Long](Limbs)
      val wideDenominator = new scala.Array[Long](Limbs)
      limbsFromLong(wideNumerator, mantissa)
      limbsClear(wideDenominator)
      wideDenominator(-exponent/31) = 1L << -exponent%31
      encodeMagnitude(terms, euclideanWide(wideNumerator, wideDenominator, terms), budget)

  // Decoding a nonzero magnitude to its canonical fraction.
  private def fractionOf(magnitude: Long): (Long, Long) =
    val terms = new scala.Array[Long](TermLimit)
    continuants(terms, decodeTerms(magnitude, terms))

  // Signed addition of two decoded fractions over wide intermediates, for Q64.
  private def addWide
    ( leftNegative:  Boolean, leftNumerator: Long, leftDenominator: Long,
      rightNegative: Boolean, rightNumerator: Long, rightDenominator: Long,
      budget:        Int )
  :   (Boolean, Long) =

    val cross = new scala.Array[Long](Limbs)
    val cross2 = new scala.Array[Long](Limbs)
    val wideDenominator = new scala.Array[Long](Limbs)
    limbsMultiply(leftNumerator, rightDenominator, cross)
    limbsMultiply(rightNumerator, leftDenominator, cross2)
    limbsMultiply(leftDenominator, rightDenominator, wideDenominator)
    val terms = new scala.Array[Long](TermLimit)

    if leftNegative == rightNegative then
      limbsAdd(cross, cross2)
      (leftNegative, encodeMagnitude(terms, euclideanWide(cross, wideDenominator, terms), budget))
    else limbsCompare(cross, cross2) match
      case 0 => (false, 0L)

      case order =>
        if order > 0 then
          limbsSubtract(cross, cross2)
          ( leftNegative,
            encodeMagnitude(terms, euclideanWide(cross, wideDenominator, terms), budget) )
        else
          limbsSubtract(cross2, cross)
          ( rightNegative,
            encodeMagnitude(terms, euclideanWide(cross2, wideDenominator, terms), budget) )

  // Multiplication or division of two decoded fractions over wide intermediates, for Q64.
  private def multiplyWide(numerator: Long, factor: Long, denominator: Long, divisor: Long,
                           budget: Int): Long =
    val wideNumerator = new scala.Array[Long](Limbs)
    val wideDenominator = new scala.Array[Long](Limbs)
    limbsMultiply(numerator, factor, wideNumerator)
    limbsMultiply(denominator, divisor, wideDenominator)
    val terms = new scala.Array[Long](TermLimit)
    encodeMagnitude(terms, euclideanWide(wideNumerator, wideDenominator, terms), budget)

  // Saturation bound for parsed digit strings; values this size already encode to the
  // largest representable integer.
  private val Saturated: Long = 1L << 62

  // The grammar `[+|-] digits [. digits] [(e|E) [+|-] digits]` or `[+|-] digits / digits`,
  // with the value returned as a sign and a saturating fraction; `Unset` for anything else.
  // A zero denominator is well-formed — `1/0` — and builds NaR downstream.
  private def parsedRational(text: Text): Optional[(Boolean, Long, Long)] =
    val input: String = text.s
    val length = input.length
    var index = 0
    var negative = false

    if index < length && (input.charAt(index) == '+' || input.charAt(index) == '-') then
      if input.charAt(index) == '-' then negative = true
      index += 1

    var numerator = 0L
    var digits = false

    while index < length && input.charAt(index).isDigit do
      numerator = math.min(numerator*10 + (input.charAt(index) - '0'), Saturated)
      digits = true
      index += 1

    if !digits then Unset
    else if index < length && input.charAt(index) == '/' then
      index += 1
      var denominator = 0L
      var digits2 = false

      while index < length && input.charAt(index).isDigit do
        denominator = math.min(denominator*10 + (input.charAt(index) - '0'), Saturated)
        digits2 = true
        index += 1

      if digits2 && index == length then (negative, numerator, denominator) else Unset
    else
      var scale = 0

      if index < length && input.charAt(index) == '.' then
        index += 1
        var digits2 = false

        while index < length && input.charAt(index).isDigit do
          if numerator < Saturated && scale < 18 then
            numerator = numerator*10 + (input.charAt(index) - '0')
            scale += 1

          digits2 = true
          index += 1

        if !digits2 then return Unset

      var exponent = 0

      if index < length && (input.charAt(index) == 'e' || input.charAt(index) == 'E') then
        index += 1
        var exponentNegative = false

        if index < length && (input.charAt(index) == '+' || input.charAt(index) == '-') then
          if input.charAt(index) == '-' then exponentNegative = true
          index += 1

        if index >= length || !input.charAt(index).isDigit then return Unset

        while index < length && input.charAt(index).isDigit do
          exponent = math.min(exponent*10 + (input.charAt(index) - '0'), 999)
          index += 1

        if exponentNegative then exponent = -exponent

      if index != length then Unset else
        scale -= exponent

        while scale < 0 do
          numerator = math.min(numerator*10, Saturated)
          scale += 1

        var denominator = 1L

        while scale > 0 do
          if denominator < Saturated/10 then denominator *= 10
          else if numerator > 1L then numerator /= 10
          scale -= 1

        (negative, numerator, denominator)

  private def render(negative: Boolean, numerator: Long, denominator: Long): Text =
    val builder = StringBuilder()
    if negative then builder.append('-')
    builder.append(numerator.toString)

    if denominator != 1L then
      builder.append('/')
      builder.append(denominator.toString)

    builder.toString.tt

  private def widen32(word: Int): Q64 =
    if word == Int.MinValue then q64(Long.MinValue)
    else if word == 0 then q64(0L)
    else
      val (numerator, denominator) = fractionOf((word & Int.MaxValue).toLong)
      val magnitude = encodeFraction(numerator, denominator, Budget64)
      q64(if word < 0 then magnitude | Long.MinValue else magnitude)

  private def narrow64(word: Long): Q32 =
    if word == Long.MinValue then q32(Int.MinValue)
    else if word == 0L then q32(0)
    else
      val (numerator, denominator) = fractionOf(word & Long.MaxValue)
      val magnitude = encodeFraction(numerator, denominator, Budget32)
      if magnitude == 0L then q32(0)
      else q32(magnitude.toInt | (if word < 0L then Int.MinValue else 0))

  object Q64:
    final val Zero: Q64 = q64(0L)
    final val One: Q64 = q64(encodeFraction(1L, 1L, Budget64))
    final val Nar: Q64 = q64(Long.MinValue)
    final val Max: Q64 = q64(encodeFraction(0x7ffffffeL, 1L, Budget64))
    final val Min: Q64 = q64(Max | Long.MinValue)

    inline given underlying: Underlying[Q64, Long] = caps.unsafe.unsafeErasedValue
    inline given canEqual: CanEqual[Q64, Q64] = caps.unsafe.unsafeErasedValue

    // `Q64` dealiases to `Long` here, so arrays of it — as `mosquito`'s `Matrix` builds —
    // are primitive `long[]`s.
    given classTag: ClassTag[Q64] = summon[ClassTag[Long]].asInstanceOf[ClassTag[Q64]]

    private def build(negative: Boolean, numerator: Long, denominator: Long): Q64 =
      if denominator == 0L then q64(Long.MinValue)
      else if numerator == 0L then q64(0L)
      else
        val magnitude = encodeFraction(numerator, denominator, Budget64)
        if magnitude == 0L then q64(0L)
        else if negative then q64(magnitude | Long.MinValue)
        else q64(magnitude)

    private def clamped(value: Long): Long =
      if value == Long.MinValue then Long.MaxValue else math.abs(value)

    def apply(value: Int): Q64 = apply(value.toLong, 1L)
    def apply(value: Long): Q64 = apply(value, 1L)

    def apply(numerator: Long, denominator: Long): Q64 =
      build(numerator < 0L ^ denominator < 0L, clamped(numerator), clamped(denominator))

    def apply(value: Double): Q64 =
      if !JDouble.isFinite(value) then Nar
      else if value == 0.0 then Zero
      else
        val magnitude = doubleMagnitude(math.abs(value), Budget64)
        if magnitude == 0L then q64(0L)
        else if value < 0.0 then q64(magnitude | Long.MinValue)
        else q64(magnitude)

    def parse(text: Text): Q64 raises Rational.Error =
      if text.s == "NaR" then Nar else parsedRational(text) match
        case value: (Boolean, Long, Long) => build(value(0), value(1), value(2))
        case _                            => abort(Rational.Error(text))

    // The signed three-way comparison behind `Orderable`, with NaR below everything; the
    // relational operators themselves treat NaR as unordered.
    def comparison(left: Q64, right: Q64): Int =
      if left == right then 0
      else if left == Long.MinValue then -1
      else if right == Long.MinValue then 1
      else if left == 0L then (if right > 0L then -1 else 1)
      else if right == 0L then (if left > 0L then 1 else -1)
      else if left < 0L && right > 0L then -1
      else if left > 0L && right < 0L then 1
      else
        val (leftNumerator, leftDenominator) = fractionOf(left & Long.MaxValue)
        val (rightNumerator, rightDenominator) = fractionOf(right & Long.MaxValue)
        val cross = new scala.Array[Long](Limbs)
        val cross2 = new scala.Array[Long](Limbs)
        limbsMultiply(leftNumerator, rightDenominator, cross)
        limbsMultiply(rightNumerator, leftDenominator, cross2)
        val order = limbsCompare(cross, cross2)
        if left < 0L then -order else order

    given orderable: Q64 is Orderable:
      inline def compare
          (inline left: Q64, inline right: Q64, inline strict: Boolean, inline greater: Boolean)
      :   Boolean =
        if left == Long.MinValue || right == Long.MinValue then false else
          val result = comparison(left, right)

          if greater then (if strict then result > 0 else result >= 0)
          else (if strict then result < 0 else result <= 0)

    // `val ratio: Q64 = 0.35` under `genericNumberLiterals`: decimal literals convert
    // exactly — 0.35 is 7/20 — whenever the fraction fits the payload.
    given fromDigits: FromDigits.Decimal[Q64] = digits =>
      parsedRational(digits.tt) match
        case value: (Boolean, Long, Long) => build(value(0), value(1), value(2))
        case _                            => throw FromDigits.MalformedNumber(digits)

    given textualizable: Q64 is Textualizable = value => value.text

    private def negation(value: Q64): Q64 =
      if value == 0L || value == Long.MinValue then value else q64(value ^ Long.MinValue)

    private def sum(left: Q64, right: Q64): Q64 =
      if left == Long.MinValue || right == Long.MinValue then q64(Long.MinValue)
      else if left == 0L then right
      else if right == 0L then left
      else
        val (leftNumerator, leftDenominator) = fractionOf(left & Long.MaxValue)
        val (rightNumerator, rightDenominator) = fractionOf(right & Long.MaxValue)

        val (negative, magnitude) = addWide
          ( left < 0L, leftNumerator, leftDenominator,
            right < 0L, rightNumerator, rightDenominator,
            Budget64 )

        if magnitude == 0L then q64(0L)
        else if negative then q64(magnitude | Long.MinValue)
        else q64(magnitude)

    private def product(left: Q64, right: Q64, inverted: Boolean): Q64 =
      if left == Long.MinValue || right == Long.MinValue then q64(Long.MinValue)
      else if inverted && right == 0L then q64(Long.MinValue)
      else if left == 0L || right == 0L then q64(0L)
      else
        val (leftNumerator, leftDenominator) = fractionOf(left & Long.MaxValue)
        val (rightNumerator, rightDenominator) = fractionOf(right & Long.MaxValue)

        val magnitude =
          if inverted
          then multiplyWide(leftNumerator, rightDenominator, leftDenominator, rightNumerator,
                            Budget64)
          else multiplyWide(leftNumerator, rightNumerator, leftDenominator, rightDenominator,
                            Budget64)

        if magnitude == 0L then q64(0L)
        else if left < 0L ^ right < 0L then q64(magnitude | Long.MinValue)
        else q64(magnitude)

    // Statistical operations skolemize the element type they work over, and `Self` is
    // invariant in a typeclass, so — as `abacist`'s `Quanta` does — the arithmetic givens
    // take any subtype of Q64 as `Self`, with `refine` casting the computed word back.
    private def refine[self](value: Q64): self = value.asInstanceOf[self]

    // The standard arithmetic operators come from symbolism, so `+`, `-`, `*`, `/`, prefix
    // negation and `sqrt` resolve through the generic operators without any Q64-specific
    // exports.
    given addable: [self <: Q64] => self is Addable:
      type Operand = self
      type Result = self

      def add(augend: self, addend: self): self = refine(sum(augend, addend))

    given subtractable: [self <: Q64] => self is Subtractable:
      type Operand = self
      type Result = self

      def subtract(minuend: self, subtrahend: self): self =
        refine(sum(minuend, negation(subtrahend)))

    given multiplicable: [self <: Q64] => self is Multiplicable:
      type Operand = self
      type Result = self

      def multiply(multiplicand: self, multiplier: self): self =
        refine(product(multiplicand, multiplier, false))

    // Division is total: division by zero gives NaR, which propagates like `Double.NaN`.
    given divisible: [self <: Q64] => self is Divisible:
      type Operand = self
      type Result = self

      def divide(dividend: self, divisor: self): self = refine(product(dividend, divisor, true))

    given negatable: [self <: Q64] => self is Negatable to self =
      operand => refine(negation(operand))

    // The lower bound stops a `(? <: value) is Zeroic` search — as `total` makes —
    // collapsing `self` to `Nothing`, whose `zero` would throw on the cast.
    given zeroic: [self >: Q64 <: Q64] => self is Zeroic:
      inline def zero: self = refine(Zero)

    given unital: [self >: Q64 <: Q64] => self is Unital = () => refine(One)

    // The best representable approximation of the square root: exact whenever the operand
    // is the square of a representable rational, and NaR for negative operands.
    given rootable: Q64 is Rootable[2]:
      type Result = Q64

      def root(value: Q64): Q64 =
        if value < 0L then q64(Long.MinValue)
        else if value == 0L then q64(0L)
        else
          val (numerator, denominator) = fractionOf(value & Long.MaxValue)
          q64(sqrtMagnitude(numerator, denominator, Budget64))

    // Statistical operations divide and rescale by `Double`s — collection sizes, exact
    // halves — which convert to rationals exactly, so `mean`, `median` and `variance` stay
    // exact for exact inputs.
    given divisibleDouble: [self <: Q64] => self is Divisible:
      type Operand = Double
      type Result = self

      def divide(dividend: self, divisor: Double): self =
        refine(product(dividend, Q64(divisor), true))

    given multiplicableDouble: [self <: Q64] => self is Multiplicable:
      type Operand = Double
      type Result = self

      def multiply(multiplicand: self, multiplier: Double): self =
        refine(product(multiplicand, Q64(multiplier), false))

    // Scaling a rational by a whole number is exact for every `Int` — every `Int` but the
    // two extremes is representable, and those clamp to ±(2³¹−2) — and for every `Long` within
    // that same range; larger multipliers clamp, like every other out-of-budget result.
    given multiplicableInt: [self <: Q64] => self is Multiplicable:
      type Operand = Int
      type Result = self

      def multiply(multiplicand: self, multiplier: Int): self =
        refine(product(multiplicand, Q64(multiplier), false))

    given multiplicableLong: [self <: Q64] => self is Multiplicable:
      type Operand = Long
      type Result = self

      def multiply(multiplicand: self, multiplier: Long): self =
        refine(product(multiplicand, Q64(multiplier), false))

    // Dividing by a whole number is as exact as multiplying by one, and division by zero
    // gives NaR, as it does for every other operand type.
    given divisibleInt: [self <: Q64] => self is Divisible:
      type Operand = Int
      type Result = self

      def divide(dividend: self, divisor: Int): self =
        refine(product(dividend, Q64(divisor), true))

    given divisibleLong: [self <: Q64] => self is Divisible:
      type Operand = Long
      type Result = self

      def divide(dividend: self, divisor: Long): self =
        refine(product(dividend, Q64(divisor), true))

    // Destructuring through symbolism's `/:` extractor: `case n /: d => …`.
    given quotient: Q64 is Quotient:
      type Topic = Long
      type Transport = Long

      def decompose(division: Q64): scala.Option[(Long, Long)] =
        if division == Long.MinValue then scala.None
        else scala.Some((division.numerator, division.denominator))

    // Implicit conversions exist only where exact, matching `F64`'s exclusion of `Long`;
    // the two extremes `Int.MaxValue` and `Int.MinValue` round by one to ±(2³¹−2).
    inline given intConversion: Conversion[Int, Q64]:
      def apply(value: Int): Q64 = Q64(value.toLong, 1L)

    inline given shortConversion: Conversion[Short, Q64]:
      def apply(value: Short): Q64 = Q64(value.toLong, 1L)

    inline given byteConversion: Conversion[Byte, Q64]:
      def apply(value: Byte): Q64 = Q64(value.toLong, 1L)

    // Widening from `Q32` is always exact.
    inline given q32Conversion: Conversion[Q32, Q64]:
      def apply(value: Q32): Q64 = widen32(value)

    extension (left: Q64)
      def numerator: Long =
        if left == Long.MinValue || left == 0L then 0L else
          val (numerator, _) = fractionOf(left & Long.MaxValue)
          if left < 0L then -numerator else numerator

      // NaR has denominator 0, the unique word without one.
      def denominator: Long =
        if left == Long.MinValue then 0L
        else if left == 0L then 1L
        else
          val (_, denominator) = fractionOf(left & Long.MaxValue)
          denominator

      def signum: Int =
        if left == Long.MinValue || left == 0L then 0 else if left < 0L then -1 else 1

      def nar: Boolean = left == Long.MinValue
      def abs: Q64 = if left == Long.MinValue then left else q64(left & Long.MaxValue)
      def whole: Boolean = left.denominator == 1L

      def reciprocal: Q64 =
        if left == Long.MinValue || left == 0L then q64(Long.MinValue) else
          val (numerator, denominator) = fractionOf(left & Long.MaxValue)
          val magnitude = encodeFraction(denominator, numerator, Budget64)
          if magnitude == 0L then q64(0L)
          else if left < 0L then q64(magnitude | Long.MinValue)
          else q64(magnitude)

      def floor: Q64 =
        if left == Long.MinValue || left == 0L then left else
          val (numerator, denominator) = fractionOf(left & Long.MaxValue)

          if left > 0L then build(false, numerator/denominator, 1L)
          else build(true, numerator/denominator + (if numerator%denominator == 0L then 0L else 1L),
                     1L)

      def ceiling: Q64 =
        if left == Long.MinValue || left == 0L then left else
          val (numerator, denominator) = fractionOf(left & Long.MaxValue)

          if left < 0L then build(true, numerator/denominator, 1L)
          else build(false, numerator/denominator + (if numerator%denominator == 0L then 0L else 1L),
                     1L)

      // Half-up rounding, matching `math.round` on `Double`.
      def round: Long =
        if left == Long.MinValue || left == 0L then 0L else
          val (numerator, denominator) = fractionOf(left & Long.MaxValue)
          val signed = if left < 0L then -numerator else numerator
          math.floorDiv(2L*signed + denominator, 2L*denominator)

      // Numerator and denominator are both below 2⁵³, so the quotient is correctly rounded.
      def double: Double =
        if left == Long.MinValue then Double.NaN
        else if left == 0L then 0.0
        else
          val (numerator, denominator) = fractionOf(left & Long.MaxValue)
          val quotient = numerator.toDouble/denominator.toDouble
          if left < 0L then -quotient else quotient

      def text: Text =
        if left == Long.MinValue then "NaR".tt
        else if left == 0L then "0".tt
        else
          val (numerator, denominator) = fractionOf(left & Long.MaxValue)
          render(left < 0L, numerator, denominator)

      // The best `Q32` approximation; exact whenever the value fits `Q32`'s budget.
      def q32: Q32 = narrow64(left)

  object Q32:
    final val Zero: Q32 = q32(0)
    final val One: Q32 = q32(encodeFraction(1L, 1L, Budget32).toInt)
    final val Nar: Q32 = q32(Int.MinValue)
    final val Max: Q32 = q32(encodeFraction(0x7ffeL, 1L, Budget32).toInt)
    final val Min: Q32 = q32(Max | Int.MinValue)

    inline given underlying: Underlying[Q32, Int] = caps.unsafe.unsafeErasedValue
    inline given canEqual: CanEqual[Q32, Q32] = caps.unsafe.unsafeErasedValue

    // `Q32` dealiases to `Int` here, so arrays of it are primitive `int[]`s.
    given classTag: ClassTag[Q32] = summon[ClassTag[Int]].asInstanceOf[ClassTag[Q32]]

    private def build(negative: Boolean, numerator: Long, denominator: Long): Q32 =
      if denominator == 0L then q32(Int.MinValue)
      else if numerator == 0L then q32(0)
      else
        val magnitude = encodeFraction(numerator, denominator, Budget32)
        if magnitude == 0L then q32(0)
        else if negative then q32(magnitude.toInt | Int.MinValue)
        else q32(magnitude.toInt)

    private def clamped(value: Long): Long =
      if value == Long.MinValue then Long.MaxValue else math.abs(value)

    def apply(value: Int): Q32 = apply(value.toLong, 1L)
    def apply(value: Long): Q32 = apply(value, 1L)

    def apply(numerator: Long, denominator: Long): Q32 =
      build(numerator < 0L ^ denominator < 0L, clamped(numerator), clamped(denominator))

    def apply(value: Double): Q32 =
      if !JDouble.isFinite(value) then Nar
      else if value == 0.0 then Zero
      else
        val magnitude = doubleMagnitude(math.abs(value), Budget32)
        if magnitude == 0L then q32(0)
        else if value < 0.0 then q32(magnitude.toInt | Int.MinValue)
        else q32(magnitude.toInt)

    def parse(text: Text): Q32 raises Rational.Error =
      if text.s == "NaR" then Nar else parsedRational(text) match
        case value: (Boolean, Long, Long) => build(value(0), value(1), value(2))
        case _                            => abort(Rational.Error(text))

    // The signed three-way comparison behind `Orderable`, with NaR below everything; the
    // relational operators themselves treat NaR as unordered. Decoded numerators and
    // denominators stay below 2²¹, so the cross-products need only `Long`s.
    def comparison(left: Q32, right: Q32): Int =
      if left == right then 0
      else if left == Int.MinValue then -1
      else if right == Int.MinValue then 1
      else if left == 0 then (if right > 0 then -1 else 1)
      else if right == 0 then (if left > 0 then 1 else -1)
      else if left < 0 && right > 0 then -1
      else if left > 0 && right < 0 then 1
      else
        val (leftNumerator, leftDenominator) = fractionOf((left & Int.MaxValue).toLong)
        val (rightNumerator, rightDenominator) = fractionOf((right & Int.MaxValue).toLong)
        val order = JLong.compare(leftNumerator*rightDenominator, rightNumerator*leftDenominator)
        if left < 0 then -order else order

    given orderable: Q32 is Orderable:
      inline def compare
          (inline left: Q32, inline right: Q32, inline strict: Boolean, inline greater: Boolean)
      :   Boolean =
        if left == Int.MinValue || right == Int.MinValue then false else
          val result = comparison(left, right)

          if greater then (if strict then result > 0 else result >= 0)
          else (if strict then result < 0 else result <= 0)

    given fromDigits: FromDigits.Decimal[Q32] = digits =>
      parsedRational(digits.tt) match
        case value: (Boolean, Long, Long) => build(value(0), value(1), value(2))
        case _                            => throw FromDigits.MalformedNumber(digits)

    given textualizable: Q32 is Textualizable = value => value.text

    private def negation(value: Q32): Q32 =
      if value == 0 || value == Int.MinValue then value else q32(value ^ Int.MinValue)

    private def sum(left: Q32, right: Q32): Q32 =
      if left == Int.MinValue || right == Int.MinValue then q32(Int.MinValue)
      else if left == 0 then right
      else if right == 0 then left
      else
        val (leftNumerator, leftDenominator) = fractionOf((left & Int.MaxValue).toLong)
        val (rightNumerator, rightDenominator) = fractionOf((right & Int.MaxValue).toLong)
        val cross = leftNumerator*rightDenominator
        val cross2 = rightNumerator*leftDenominator
        val denominator = leftDenominator*rightDenominator
        val negative = left < 0

        if left < 0 == right < 0 then build(negative, cross + cross2, denominator)
        else if cross == cross2 then q32(0)
        else if cross > cross2 then build(negative, cross - cross2, denominator)
        else build(right < 0, cross2 - cross, denominator)

    private def product(left: Q32, right: Q32, inverted: Boolean): Q32 =
      if left == Int.MinValue || right == Int.MinValue then q32(Int.MinValue)
      else if inverted && right == 0 then q32(Int.MinValue)
      else if left == 0 || right == 0 then q32(0)
      else
        val (leftNumerator, leftDenominator) = fractionOf((left & Int.MaxValue).toLong)
        val (rightNumerator, rightDenominator) = fractionOf((right & Int.MaxValue).toLong)

        val (numerator, denominator) =
          if inverted then (leftNumerator*rightDenominator, leftDenominator*rightNumerator)
          else (leftNumerator*rightNumerator, leftDenominator*rightDenominator)

        build(left < 0 ^ right < 0, numerator, denominator)

    // Statistical operations skolemize the element type they work over, and `Self` is
    // invariant in a typeclass, so — as `abacist`'s `Quanta` does — the arithmetic givens
    // take any subtype of Q32 as `Self`, with `refine` casting the computed word back.
    private def refine[self](value: Q32): self = value.asInstanceOf[self]

    // The standard arithmetic operators come from symbolism, so `+`, `-`, `*`, `/`, prefix
    // negation and `sqrt` resolve through the generic operators without any Q32-specific
    // exports.
    given addable: [self <: Q32] => self is Addable:
      type Operand = self
      type Result = self

      def add(augend: self, addend: self): self = refine(sum(augend, addend))

    given subtractable: [self <: Q32] => self is Subtractable:
      type Operand = self
      type Result = self

      def subtract(minuend: self, subtrahend: self): self =
        refine(sum(minuend, negation(subtrahend)))

    given multiplicable: [self <: Q32] => self is Multiplicable:
      type Operand = self
      type Result = self

      def multiply(multiplicand: self, multiplier: self): self =
        refine(product(multiplicand, multiplier, false))

    // Division is total: division by zero gives NaR, which propagates like `Double.NaN`.
    given divisible: [self <: Q32] => self is Divisible:
      type Operand = self
      type Result = self

      def divide(dividend: self, divisor: self): self = refine(product(dividend, divisor, true))

    given negatable: [self <: Q32] => self is Negatable to self =
      operand => refine(negation(operand))

    // The lower bound stops a `(? <: value) is Zeroic` search — as `total` makes —
    // collapsing `self` to `Nothing`, whose `zero` would throw on the cast.
    given zeroic: [self >: Q32 <: Q32] => self is Zeroic:
      inline def zero: self = refine(Zero)

    given unital: [self >: Q32 <: Q32] => self is Unital = () => refine(One)

    // The best representable approximation of the square root: exact whenever the operand
    // is the square of a representable rational, and NaR for negative operands.
    given rootable: Q32 is Rootable[2]:
      type Result = Q32

      def root(value: Q32): Q32 =
        if value < 0 then q32(Int.MinValue)
        else if value == 0 then q32(0)
        else
          val (numerator, denominator) = fractionOf((value & Int.MaxValue).toLong)
          q32(sqrtMagnitude(numerator, denominator, Budget32).toInt)

    given divisibleDouble: [self <: Q32] => self is Divisible:
      type Operand = Double
      type Result = self

      def divide(dividend: self, divisor: Double): self =
        refine(product(dividend, Q32(divisor), true))

    given multiplicableDouble: [self <: Q32] => self is Multiplicable:
      type Operand = Double
      type Result = self

      def multiply(multiplicand: self, multiplier: Double): self =
        refine(product(multiplicand, Q32(multiplier), false))

    // Exact for multipliers within ±32766, `Q32`'s integer range; larger ones clamp, like
    // every other out-of-budget result.
    given multiplicableInt: [self <: Q32] => self is Multiplicable:
      type Operand = Int
      type Result = self

      def multiply(multiplicand: self, multiplier: Int): self =
        refine(product(multiplicand, Q32(multiplier), false))

    given multiplicableLong: [self <: Q32] => self is Multiplicable:
      type Operand = Long
      type Result = self

      def multiply(multiplicand: self, multiplier: Long): self =
        refine(product(multiplicand, Q32(multiplier), false))

    // Dividing by a whole number is as exact as multiplying by one, and division by zero
    // gives NaR, as it does for every other operand type.
    given divisibleInt: [self <: Q32] => self is Divisible:
      type Operand = Int
      type Result = self

      def divide(dividend: self, divisor: Int): self =
        refine(product(dividend, Q32(divisor), true))

    given divisibleLong: [self <: Q32] => self is Divisible:
      type Operand = Long
      type Result = self

      def divide(dividend: self, divisor: Long): self =
        refine(product(dividend, Q32(divisor), true))

    // Destructuring through symbolism's `/:` extractor: `case n /: d => …`.
    given quotient: Q32 is Quotient:
      type Topic = Long
      type Transport = Long

      def decompose(division: Q32): scala.Option[(Long, Long)] =
        if division == Int.MinValue then scala.None
        else scala.Some((division.numerator, division.denominator))

    // Implicit conversions exist only where exact; the two extremes `Short.MaxValue` and
    // `Short.MinValue` round by one to ±32766.
    inline given shortConversion: Conversion[Short, Q32]:
      def apply(value: Short): Q32 = Q32(value.toLong, 1L)

    inline given byteConversion: Conversion[Byte, Q32]:
      def apply(value: Byte): Q32 = Q32(value.toLong, 1L)

    extension (left: Q32)
      def numerator: Long =
        if left == Int.MinValue || left == 0 then 0L else
          val (numerator, _) = fractionOf((left & Int.MaxValue).toLong)
          if left < 0 then -numerator else numerator

      // NaR has denominator 0, the unique word without one.
      def denominator: Long =
        if left == Int.MinValue then 0L
        else if left == 0 then 1L
        else
          val (_, denominator) = fractionOf((left & Int.MaxValue).toLong)
          denominator

      def signum: Int = if left == Int.MinValue || left == 0 then 0 else if left < 0 then -1 else 1
      def nar: Boolean = left == Int.MinValue
      def abs: Q32 = if left == Int.MinValue then left else q32(left & Int.MaxValue)
      def whole: Boolean = left.denominator == 1L

      def reciprocal: Q32 =
        if left == Int.MinValue || left == 0 then q32(Int.MinValue) else
          val (numerator, denominator) = fractionOf((left & Int.MaxValue).toLong)
          val magnitude = encodeFraction(denominator, numerator, Budget32)
          if magnitude == 0L then q32(0)
          else if left < 0 then q32(magnitude.toInt | Int.MinValue)
          else q32(magnitude.toInt)

      def floor: Q32 =
        if left == Int.MinValue || left == 0 then left else
          val (numerator, denominator) = fractionOf((left & Int.MaxValue).toLong)

          if left > 0 then build(false, numerator/denominator, 1L)
          else build(true, numerator/denominator + (if numerator%denominator == 0L then 0L else 1L),
                     1L)

      def ceiling: Q32 =
        if left == Int.MinValue || left == 0 then left else
          val (numerator, denominator) = fractionOf((left & Int.MaxValue).toLong)

          if left < 0 then build(true, numerator/denominator, 1L)
          else build(false, numerator/denominator + (if numerator%denominator == 0L then 0L else 1L),
                     1L)

      // Half-up rounding, matching `math.round` on `Double`.
      def round: Long =
        if left == Int.MinValue || left == 0 then 0L else
          val (numerator, denominator) = fractionOf((left & Int.MaxValue).toLong)
          val signed = if left < 0 then -numerator else numerator
          math.floorDiv(2L*signed + denominator, 2L*denominator)

      // Numerator and denominator are both below 2⁵³, so the quotient is correctly rounded.
      def double: Double =
        if left == Int.MinValue then Double.NaN
        else if left == 0 then 0.0
        else
          val (numerator, denominator) = fractionOf((left & Int.MaxValue).toLong)
          val quotient = numerator.toDouble/denominator.toDouble
          if left < 0 then -quotient else quotient

      def text: Text =
        if left == Int.MinValue then "NaR".tt
        else if left == 0 then "0".tt
        else
          val (numerator, denominator) = fractionOf((left & Int.MaxValue).toLong)
          render(left < 0, numerator, denominator)

      // Widening to `Q64` is always exact.
      def q64: Q64 = widen32(left)
