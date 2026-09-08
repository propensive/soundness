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
package delicious

import anticipation.*
import gossamer.*
import vacuous.*

object Placeholder:
  /** The reserved prefix of placeholder literal types. A genuine string
   *  literal type starting with this prefix is escaped by the compiler with
   *  the `esc:` form. */
  final val Prefix: Text = "⟨scala-diag:"

  def text(id: Int): Text = t"$Prefix$id⟩"

  /** The placeholder id, if the text is a placeholder reference. */
  def reference(text: Text): Optional[Int] =
    if text.s.startsWith(Prefix.s) && text.s.endsWith("⟩") then
      val body = text.s.substring(Prefix.s.length, text.s.length - 1).nn
      if body.nonEmpty && body.forall(_.isDigit) then body.toInt else Unset
    else Unset

  /** The original string literal, if the text is an escaped genuine literal. */
  def escaped(text: Text): Optional[Text] =
    val prefix = t"${Prefix}esc:"
    if text.s.startsWith(prefix.s) && text.s.endsWith("⟩")
    then text.s.substring(prefix.s.length, text.s.length - 1).nn.tt
    else Unset

  def decode(text: Text): Optional[Placeholder] =
    text.cut("|") match
      case List(id, kind, name, arity, definedAt, printed) =>
        def field(value: Text): Text = Markup.decode(value)

        try
          Placeholder
            ( field(id).s.toInt,
              PlaceholderKind(field(kind)),
              field(name),
              field(arity).s.toInt,
              if definedAt.s.isEmpty then Unset else field(definedAt),
              field(printed) )
        catch case _: NumberFormatException => Unset

      case _ => Unset

case class Placeholder
     (id:        Int,
      kind:      PlaceholderKind,
      name:      Text,
      arity:     Int,
      definedAt: Optional[Text],
      printed:   Text)
