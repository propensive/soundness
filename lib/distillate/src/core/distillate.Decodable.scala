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
package distillate

import scala.reflect

import scala.caps

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*

trait Decodable2:
  given generic: [value] => value is Decodable in value = identity(_)

object Decodable extends Decodable2:
  // The SAM instances below raise through their resolution-scoped tactic, which shares each
  // instance's given-resolution lifetime: honest capabilities (every given that includes a
  // tactic is a capability; Jon, 2026-07-12). See rep/DECISIONS.md.
  given int: (number: Tactic[Number.Error]^) => ((Int is Decodable in Text)^{number, caps.any}) =
    text =>
      try Integer.parseInt(text.s) catch case _: NumberFormatException =>
        abort(Number.Error(text, Int, Number.Error.Reason.Unparseable))

  given byte: (tactic: Tactic[Number.Error]^)
  =>  ((Byte is Decodable in Text)^{tactic, caps.any}) =
    text =>
      val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
        abort(Number.Error(text, Byte, Number.Error.Reason.Unparseable))

      if int < Byte.MinValue || int > Byte.MaxValue
      then abort(Number.Error(text, Byte, Number.Error.Reason.OutOfRange))
      else int.toByte

  given short: (tactic: Tactic[Number.Error]^)
  =>  ((Short is Decodable in Text)^{tactic, caps.any}) =
    text =>
      val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
        abort(Number.Error(text, Short, Number.Error.Reason.Unparseable))

      if int < Short.MinValue || int > Short.MaxValue
      then abort(Number.Error(text, Short, Number.Error.Reason.OutOfRange))
      else int.toShort

  given long: (tactic: Tactic[Number.Error]^)
  =>  ((Long is Decodable in Text)^{tactic, caps.any}) =
    text =>
      try java.lang.Long.parseLong(text.s) catch case _: NumberFormatException =>
        abort(Number.Error(text, Long, Number.Error.Reason.Unparseable))

  given double: (tactic: Tactic[Number.Error]^)
  =>  ((Double is Decodable in Text)^{tactic, caps.any}) =
    text =>
      try java.lang.Double.parseDouble(text.s) catch case _: NumberFormatException =>
        abort(Number.Error(text, Double, Number.Error.Reason.Unparseable))

  given float: (tactic: Tactic[Number.Error]^)
  =>  ((Float is Decodable in Text)^{tactic, caps.any}) =
    text =>
      try java.lang.Float.parseFloat(text.s) catch case _: NumberFormatException =>
        abort(Number.Error(text, Float, Number.Error.Reason.Unparseable))

  given char: Char is Decodable in Text = _.s.charAt(0)

  given enumeration: [enumeration <: reflect.Enum: {Enumerable, Identifiable as identifiable}]
  =>  (tactic: Tactic[Variant.Error]^)
  =>  ((enumeration is Decodable in Text)^{tactic, caps.any}) =
    value =>

      enumeration.value(identifiable.decode(value)).or:
        val names = enumeration.values.to[List].map(enumeration.name(_)).map(enumeration.encode(_))
        abort(Variant.Error(value, enumeration.name, names))

trait Decodable extends Typeclass, Formal, Locative:
  inline def decodable: this.type = this
  def decoded(value: Form): Self

  // Identity by default. Typeclass instances that track positional focus
  // values (see Jacinta's `JsonDecodable`) override this to enrich a focus
  // with information derivable from the `Form` after decoding. `Locus`
  // stays abstract so each instance specifies its own focus type.
  def position(value: Form, focus: Locus): Locus = focus

  def map[self2](lambda: Self => self2): (self2 is Decodable in Form)^{this, lambda} =
    value => lambda(decodable.decoded(value))
