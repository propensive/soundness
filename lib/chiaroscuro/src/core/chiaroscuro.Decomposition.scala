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
┃    Soundness, version 0.33.0.                                                                    ┃
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
package chiaroscuro

import anticipation.*
import denominative.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.reflect.*

object Decomposition:
  def apply(optional: Optional[Decomposition]): Decomposition = optional.or:
    Decomposition.Primitive(t"Unset", t"Unset", Unset)

enum Decomposition:
  case Primitive(typeName: Text, value: Text, ref: Any)
  case Product(name: Text, values: Map[Text, Decomposition], ref: Any)
  case Sequence(values: IArray[Decomposition], ref: Any)
  case Sum(name: Text, value: Decomposition, ref: Any)

  def ref: Any

  def text2: Text = this match
    case Primitive(_, text, _) => text
    case Sum(name, value, _)   => t"$name:${value.text}"
    case Sequence(values, _)   => values.map(_.text).join(t"[", t", ", t"]")

    case Product(name, values, _) =>
      t"$name(${values.map { (key, value) => t"$key: ${value.text}" }.join(t", ")}"

  def short: Text = this match
    case Primitive(_, text, _) => text
    case Sum(name, value, _)   => t"$name:${value.short}"
    case Sequence(values, _)   => t"[..${values.length}..]"
    case Product(name, _, _)   => name

  def text: Text =
    Text.construct:
      multiline(0)
      builder.toString.tt

  def multiline(indent: Int = 0, newline: Boolean = true)(using TextBuilder): Unit =
    val space = t"  "
    this match
      case Primitive(typeName, text, _) =>
        append(t"$text")

      case Sum(name, value, _) =>
        if newline then
          append(t"\n")
          append(space*indent)

        append(t"$name╱")
        value.multiline(indent, false)

      case Sequence(values, _) =>
        if newline then
          append(t"\n")
          append(space*indent)

        val last = values.length

        values.each: item =>
          append(ordinal.n0.show)
          append(t": ")
          item.multiline(indent + 1, true)
          if ordinal.n0 < last - 1 then
            append(t"\n"+(space*indent))

      case Product(name, values, _) =>
        if newline then
          append(t"\n")
          append(space*indent)

        append(t"$name:")
        val last = values.size
        append(t"\n"+(space*indent))

        values.each: (key, value) =>
          append(t"$space$key:")
          value.multiline(indent + 2, true)
          if ordinal.n0 < last - 1 then
            append(t"\n"+(space*indent))
