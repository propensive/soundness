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
┃    Soundness, version 0.36.0.                                                                    ┃
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
package gossamer

import anticipation.*
import hypotenuse.*
import proscenium.*
import rudiments.*
import vacuous.*

case class Decimalizer
   (significantFigures: Optional[Int]  = Unset,
    decimalPlaces:      Optional[Int]  = Unset,
    decimalPoint:       Char           = '.',
    minusSign:          Char           = '-',
    plusSign:           Optional[Char] = Unset,
    exponent:           Text           = "×10".tt,
    exponentThreshold:  Optional[Int]  = 3,
    superscript:        Boolean        = true,
    exponentMultiple:   Int            = 1,
    infinity:           Text           = "\u221e".tt,
    nan:                Text           = "\u2209\u211d".tt)
extends DecimalConverter:

  def exponentScale(i: Int, a: Int): Int = if i == 0 then a else exponentScale(i/10, a + 1)

  def decimalize(double: Double): Text =
    if double.isFinite then
      val negative: Boolean = double < 0.0
      val abs: Double = double.abs

      val baseScale: Int = if double == 0 then 0 else log10(abs).floor.double.toInt
      val exponentiate = exponentThreshold.lay(false)(baseScale.abs >= _)
      val exponentValue = if exponentiate then (baseScale/exponentMultiple)*exponentMultiple else 0

      val scale = baseScale - exponentValue

      val norm: Double = abs*(10 ** -baseScale)
      val digits: Int = significantFigures.or(decimalPlaces.let(1 + scale + _)).or(3)
      val sign = (negative || plusSign.present) && double != 0.0

      @tailrec
      def write(chars: Array[Char], bcd: Long, idx: Int, carry: Boolean, point: Int): Array[Char] =
        if idx >= 0 then
          var digit = bcd & 15
          var carry2 = carry

          if idx == point then chars(idx) = decimalPoint else
            if carry then digit += 1
            if digit == 10 then chars(idx) = '0' else
              carry2 = false
              chars(idx) = (digit + '0').toChar

          write(chars, if idx != point then (bcd >> 4) else bcd, idx - 1, carry2, point)
        else chars

      @tailrec
      def recur(focus: Double, bcd: Long, idx: Int): Array[Char] =
        val digit = focus.toLong
        val next: Double = (focus - digit)*10
        val bcd2 = (bcd << 4) + focus.toLong

        if digits <= idx then
          val shift = (scale - digits + 1).max(0)
          val point = scale.max(0) + 1
          val length = shift + idx - scale.min(0)

          val suffix: Int =
            if exponentiate then
              exponent.length + (if exponentValue < 0 then 1 else 0)
              + (exponentScale(exponentValue, 0))
            else 0
          val fullLength = (if sign then 1 else 0) + (if point < length then 1 else 0) + length
          val array = new Array[Char](fullLength + suffix)

          if exponentiate then
            var i = 0
            while i < exponent.length do
              array(i + fullLength) = exponent.s.charAt(i)
              i += 1

            if exponentValue < 0 then array(i + fullLength) = if superscript then '¯' else '-'
            i = fullLength + suffix - 1
            var exp = exponentValue.abs

            while exp > 0 do
              val d = exp%10
              array(i) = if !superscript then ('0' + d).toChar else d match
                case 1     => '\u00b9'
                case 2 | 3 => ('\u00b0' + d).toChar
                case d     => ('⁰' + d).toChar

              exp /= 10
              i -= 1

          write
           (array,
            bcd2 << shift*4,
            fullLength - 1,
            next >= 5,
            if sign then point + 1 else point)

        else recur(next, bcd2, idx + 1)

      val chars: Array[Char] = recur(norm, 0L, 1)
      if sign then chars(0) = (if negative then minusSign else plusSign.vouch)

      Text(new String(chars))
    else if double.isNaN then nan
    else if double.isNegInfinity then s"$minusSign$infinity".tt else infinity
