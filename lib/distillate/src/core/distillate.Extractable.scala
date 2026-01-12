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
┃    Soundness, version 0.50.0.                                                                    ┃
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
package distillate

import scala.deriving.*
import scala.reflect.*

import anticipation.*
import contingency.*
import prepositional.*
import vacuous.*

object Extractable:
  given decodable: [text <: Text, result]
        => (decodable: Tactic[Exception] ?=> result is Decodable in Text)
        =>  text is Extractable to result =
     value => safely(decodable(using strategies.throwUnsafely).decoded(value))

  given optional: [result: Extractable]
        =>  Optional[result] is Extractable to result.Result =
    value => value.let(result.extract(_))

  given irrefutable: [result] => (irrefutable: Text is Irrefutable to result)
        =>  String is Irrefutable to result =
    value => irrefutable.unapply(value.tt)

  given textChar: [text <: Text] => text is Extractable to Char =
    text => if text.s.length == 1 then text.s.head else Unset

  given textByte: [text <: Text] => text is Extractable to Byte =
    text => try text.s.toByte catch case _: NumberFormatException => Unset

  given textShort: [text <: Text] => text is Extractable to Short =
    text => try text.s.toShort catch case _: NumberFormatException => Unset

  given textInt: [text <: Text] => text is Extractable to Int =
    text => try text.s.toInt catch case e: NumberFormatException => Unset

  given textLong: [text <: Text] => text is Extractable to Long =
    text => try text.s.toLong catch case e: NumberFormatException => Unset

  given textFloat: [text <: Text] => text is Extractable to Float = text =>
    try text.s.toFloat catch case e: NumberFormatException => Unset

  given textDouble: [text <: Text] => text is Extractable to Double = text =>
    try text.s.toDouble catch case _: NumberFormatException => Unset

  given textBoolean: [text <: Text] => text is Extractable to Boolean = v =>
    if v.s == "true" then true else if v.s == "false" then false else Unset

  given shortByte: [short <: Short] => short is Extractable to Byte =
    short => short.toByte.unless(value.toInt != short)

  given intByte: [int <: Int] => int is Extractable to Byte =
    int => int.toByte.unless(value.toInt != int)

  given intShort: [int <: Int] => int is Extractable to Short =
    int => int.toShort.unless(value.toInt != int)

  given intFloat: [int <: Int] => int is Extractable to Float =
    int => int.toFloat.unless(value.toInt != int)

  given longByte: [long <: Long] => long is Extractable to Byte =
    long => long.toByte.unless(value.toLong != long)

  given longShort: [long <: Long] => long is Extractable to Short =
    long => long.toShort.unless(value.toLong != long)

  given longInt: [long <: Long] => long is Extractable to Int =
    long => long.toInt.unless(value.toLong != long)

  given longFloat: [long <: Long] => long is Extractable to Float =
    long => long.toFloat.unless(value.toLong != long)

  given longDouble: [long <: Long] => long is Extractable to Double =
    long => long.toDouble.unless(value.toLong != long)

  given floatByte: [float <: Float] => float is Extractable to Byte =
    float => float.toByte.unless(value.toFloat != float)

  given floatShort: [float <: Float] => float is Extractable to Short =
    float => float.toShort.unless(value.toFloat != float)

  given floatInt: [float <: Float] => float is Extractable to Int =
    float => float.toInt.unless(value.toFloat != float)

  given floatLong: [float <: Float] => float is Extractable to Long =
    float => float.toLong.unless(value.toFloat != float)

  given doubleByte: [double <: Double] => double is Extractable to Byte =
    double => double.toByte.unless(value.toDouble != double)

  given doubleShort: [double <: Double] => double is Extractable to Short =
    double => double.toShort.unless(value.toDouble != double)

  given doubleInt: [double <: Double] => double is Extractable to Int =
    double => double.toInt.unless(value.toDouble != double)

  given doubleLong: [double <: Double] => double is Extractable to Long =
    double => double.toLong.unless(value.toDouble != double)

  given doubleFloat: [double <: Double] => double is Extractable to Float =
    double => double.toFloat.unless(value.toDouble != double)

  given valueOf: [enumeration <: Enum: Mirror.SumOf as mirror, text <: Text]
        =>  text is Extractable to enumeration =
    text =>
      import Selectable.reflectiveSelectable

      mirror match
        case mirror: { def valueOf(name: String): enumeration } @unchecked =>
          try mirror.valueOf(text.s) catch case error: Exception => Unset

  given fromOrdinal: [enumeration <: Enum: Mirror.SumOf as mirror, int <: Int]
        =>  int is Extractable to enumeration =
    ordinal =>
      import Selectable.reflectiveSelectable
      mirror match
        case mirror: { def fromOrdinal(ordinal: Int): enumeration } @unchecked =>
          try mirror.fromOrdinal(ordinal) catch case error: Exception => Unset

trait Extractable extends Typeclass, Resultant:
  def extract(value: Self): Optional[Result]
  def unapply(value: Self): Option[Result] = extract(value).option
