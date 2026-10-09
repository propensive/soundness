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
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import rudiments.*
import vacuous.*

object Placeholder:
  /** The reserved prefix of placeholder literal types. A genuine string
   *  literal type starting with this prefix is escaped by the compiler with
   *  the `esc:` form. */
  final val Prefix: Text = t"⟨scala-diag:"

  def text(id: Int): Text = t"$Prefix$id⟩"

  /** The placeholder id, if the text is a placeholder reference. */
  def reference(text: Text): Optional[Int] =
    if text.starts(Prefix) && text.ends(t"⟩") then
      val body: Text = text.skip(Prefix.length).skip(1, Rtl)
      if !body.nil && body.s.forall(_.isDigit) then safely(body.as[Int]) else Unset
    else Unset

  /** The original string literal, if the text is an escaped genuine literal. */
  def escaped(text: Text): Optional[Text] =
    val prefix = t"${Prefix}esc:"
    if text.starts(prefix) && text.ends(t"⟩") then text.skip(prefix.length).skip(1, Rtl) else Unset

  def decode(text: Text): Optional[Placeholder] =
    text.cut(t"|") match
      case List(id, kind, name, arity, definedAt, printed) =>
        def field(value: Text): Text = Markup.decode(value)

        safely:
          Placeholder
            ( field(id).as[Int],
              PlaceholderKind(field(kind)),
              field(name),
              field(arity).as[Int],
              if definedAt.nil then Unset else field(definedAt),
              field(printed) )

      case _ => Unset

case class Placeholder
     ( id:        Int,
      kind:      PlaceholderKind,
      name:      Text,
      arity:     Int,
      definedAt: Optional[Text],
      printed:   Text )
