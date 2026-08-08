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
package spectacular

import scala.reflect

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import proscenium.compat.mkString
import prepositional.*
import rudiments.*
import vacuous.*

object Showable:
  given showable: [value: Textualizable] => value is Showable = value.textual(_)
  given text: [text <: Text] => text is Showable = identity(_)
  given string: String is Showable = _.tt
  given char: Char is Showable = char => char.toString.tt
  given long: Long is Showable = long => long.toString.tt
  given int: Int is Showable = int => int.toString.tt
  given short: Short is Showable = short => short.toString.tt
  given byte: Byte is Showable = byte => byte.toString.tt
  given message: Message is Showable = _.text
  given double: (decimalizer: DecimalConverter) => Double is Showable = decimalizer.decimalize(_)
  given boolean: (affirmation: Affirmation) => Boolean is Showable = affirmation(_)
  given option: [value: Showable] => Option[value] is Showable = _.fold("none".tt)(value.text(_))
  given bytes: Bytes is Showable = _.text
  given enumeration: [enumeration <: reflect.Enum] => enumeration is Showable = _.toString.tt

  given set: [element: Showable] => Set[element] is Showable =
    _.map(_.show).stdlib.mkString("{", ", ", "}").tt

  given list: [element: Showable] => List[element] is Showable =
    _.map(_.show).mkString("[", ", ", "]").tt

  given sequence: [element: Showable] => Sequence[element] is Showable =
    _.map(_.show).stdlib.mkString("[ ", " ", " ]").tt

  given none: None.type is Showable = none => "none".tt

  given specializable: Specializable is Showable = value =>
    value.getClass.nn.getName.nn.split("\\.").nn.last.nn.dropRight(1).toLowerCase.nn.tt

  given zerary: Ordinal is Showable = ordinal => s"${ordinal.n0}.₀".tt

  given typeRepr: (quotes: Quotes) => quotes.reflect.TypeRepr is Showable = repr =>
    stenography.internal.name(using repr.asType)

  given meta: [meta] => (quotes: Quotes) => Type[meta] is Showable =
    stenography.internal.name[meta](using _)


trait Showable extends Communicable:
  def text(value: Self): Text
  def message(value: Self): Message = Message(text(value))

  override def contramap[self2](lambda: self2 -> Self): self2 is Showable =
    value => text(lambda(value))
