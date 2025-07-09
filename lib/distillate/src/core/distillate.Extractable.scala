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
┃    Soundness, version 0.38.0.                                                                    ┃
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
        =>  text is Extractable into result =
     value => safely(decodable(using strategies.throwUnsafely).decoded(value))

  given optional: [result: Extractable]
        =>  Optional[result] is Extractable into result.Result =
    value => value.let(result.extract(_))

  given irrefutable: [result] => (irrefutable: Text is Irrefutable into result)
        =>  String is Irrefutable into result =
    value => irrefutable.unapply(value.tt)

  given textChar: [text <: Text] => text is Extractable into Char =
    text => if text.s.length == 1 then text.s.head else Unset

  given textByte: [text <: Text] => text is Extractable into Byte =
    text => try text.s.toByte catch case _: NumberFormatException => Unset

  given textShort: [text <: Text] => text is Extractable into Short =
    text => try text.s.toShort catch case _: NumberFormatException => Unset

  given textInt: [text <: Text] => text is Extractable into Int =
    text => try text.s.toInt catch case e: NumberFormatException => Unset

  given textLong: [text <: Text] => text is Extractable into Long =
    text => try text.s.toLong catch case e: NumberFormatException => Unset

  given textFloat: [text <: Text] => text is Extractable into Float = text =>
    try text.s.toFloat catch case e: NumberFormatException => Unset

  given textDouble: [text <: Text] => text is Extractable into Double = text =>
    try text.s.toDouble catch case _: NumberFormatException => Unset

  given textBoolean: [text <: Text] => text is Extractable into Boolean = v =>
    if v.s == "true" then true else if v.s == "false" then false else Unset

  given shortByte: [short <: Short] => short is Extractable into Byte =
    short => short.toByte.unless(value.toInt != short)

  given intByte: [int <: Int] => int is Extractable into Byte =
    int => int.toByte.unless(value.toInt != int)

  given intShort: [int <: Int] => int is Extractable into Short =
    int => int.toShort.unless(value.toInt != int)

  given intFloat: [int <: Int] => int is Extractable into Float =
    int => int.toFloat.unless(value.toInt != int)

  given longByte: [long <: Long] => long is Extractable into Byte =
    long => long.toByte.unless(value.toLong != long)

  given longShort: [long <: Long] => long is Extractable into Short =
    long => long.toShort.unless(value.toLong != long)

  given longInt: [long <: Long] => long is Extractable into Int =
    long => long.toInt.unless(value.toLong != long)

  given longFloat: [long <: Long] => long is Extractable into Float =
    long => long.toFloat.unless(value.toLong != long)

  given longDouble: [long <: Long] => long is Extractable into Double =
    long => long.toDouble.unless(value.toLong != long)

  given floatByte: [float <: Float] => float is Extractable into Byte =
    float => float.toByte.unless(value.toFloat != float)

  given floatShort: [float <: Float] => float is Extractable into Short =
    float => float.toShort.unless(value.toFloat != float)

  given floatInt: [float <: Float] => float is Extractable into Int =
    float => float.toInt.unless(value.toFloat != float)

  given floatLong: [float <: Float] => float is Extractable into Long =
    float => float.toLong.unless(value.toFloat != float)

  given doubleByte: [double <: Double] => double is Extractable into Byte =
    double => double.toByte.unless(value.toDouble != double)

  given doubleShort: [double <: Double] => double is Extractable into Short =
    double => double.toShort.unless(value.toDouble != double)

  given doubleInt: [double <: Double] => double is Extractable into Int =
    double => double.toInt.unless(value.toDouble != double)

  given doubleLong: [double <: Double] => double is Extractable into Long =
    double => double.toLong.unless(value.toDouble != double)

  given doubleFloat: [double <: Double] => double is Extractable into Float =
    double => double.toFloat.unless(value.toDouble != double)

  given valueOf: [enumeration <: Enum: Mirror.SumOf as mirror, text <: Text]
        =>  text is Extractable into enumeration =
    text =>
      import Selectable.reflectiveSelectable

      mirror match
        case mirror: { def valueOf(name: String): enumeration } @unchecked =>
          try mirror.valueOf(text.s) catch case error: Exception => Unset

  given fromOrdinal: [enumeration <: Enum: Mirror.SumOf as mirror, int <: Int]
        =>  int is Extractable into enumeration =
    ordinal =>
      import Selectable.reflectiveSelectable
      mirror match
        case mirror: { def fromOrdinal(ordinal: Int): enumeration } @unchecked =>
          try mirror.fromOrdinal(ordinal) catch case error: Exception => Unset

trait Extractable extends Typeclass, Resultant:
  def extract(value: Self): Optional[Result]
  def unapply(value: Self): Option[Result] = extract(value).option
