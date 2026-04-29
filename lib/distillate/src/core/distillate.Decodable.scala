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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import denominative.*
import digression.*
import inimitable.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*

trait Decodable2:
  given generic: [value] => value is Decodable in value = identity(_)

object Decodable extends Decodable2:
  given int: (number: Tactic[NumberError]) => Int is Decodable in Text = text =>
    try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Int, NumberError.Reason.Unparseable)) yet 0

  given fqcn: Tactic[FqcnError] => Fqcn is Decodable in Text = Fqcn(_)
  given uuid: Tactic[UuidError] => Uuid is Decodable in Text = Uuid.parse(_)

  given byte: Tactic[NumberError] => Byte is Decodable in Text = text =>
    val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Byte, NumberError.Reason.Unparseable)) yet 0

    if int < Byte.MinValue || int > Byte.MaxValue
    then raise(NumberError(text, Byte, NumberError.Reason.OutOfRange)) yet 0.toByte
    else int.toByte

  given short: Tactic[NumberError] => Short is Decodable in Text = text =>
    val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Short, NumberError.Reason.Unparseable)) yet 0

    if int < Short.MinValue || int > Short.MaxValue
    then raise(NumberError(text, Short, NumberError.Reason.OutOfRange)) yet 0.toShort
    else int.toShort

  given long: Tactic[NumberError] => Long is Decodable in Text = text =>
    try java.lang.Long.parseLong(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Long, NumberError.Reason.Unparseable)) yet 0L

  given double: Tactic[NumberError] => Double is Decodable in Text = text =>
    try java.lang.Double.parseDouble(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Double, NumberError.Reason.Unparseable)) yet 0.0

  given float: Tactic[NumberError] => Float is Decodable in Text = text =>
    try java.lang.Float.parseFloat(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Float, NumberError.Reason.Unparseable)) yet 0.0F

  given char: Char is Decodable in Text = _.s(0)


  given enumeration: [enumeration <: reflect.Enum: {Enumerable, Identifiable as identifiable}]
  =>  Tactic[VariantError]
  =>  enumeration is Decodable in Text = value =>

    enumeration.value(identifiable.decode(value)).or:
      val names = enumeration.values.to(List).map(enumeration.name(_)).map(enumeration.encode(_))
      raise(VariantError(value, enumeration.name, names)) yet enumeration.value(Prim).vouch

trait Decodable extends Typeclass, Formal:
  inline def decodable: this.type = this
  def decoded(value: Form): Self

  def map[self2](lambda: Self => self2): self2 is Decodable in Form =
    value => lambda(decodable.decoded(value))
