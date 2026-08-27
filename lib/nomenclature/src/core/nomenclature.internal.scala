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
package nomenclature

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import prepositional.*
import spectacular.*
import scala.quoted.*

object internal:
  opaque type Name[+plane] <: anticipation.Text = anticipation.Text

  object Name:
    given encodable: [plane] => Name[plane] is Encodable in Text = identity(_)

    // `Name` is a subtype of `Text`, so without an instance of its own it would reach
    // spectacular's `Text` instance and render `t"…"` — a name would be indistinguishable from
    // the text it is validated to be. The `n"…"` prefix says which of the two it is, and the
    // characters are escaped exactly as a text literal's are.
    // `Name` is covariant in its plane, so `Name[Any]` bounds every name.
    given inspectable: [name <: Name[Any]] => name is Inspectable = name =>
      val builder: StringBuilder = new StringBuilder("n\"")
      val string: String = (name: Text).s
      var index: Int = 0

      while index < string.length do
        builder.append(Inspectable.escape(string.charAt(index), true).s)
        index += 1

      builder.append('"').toString.tt

    inline given decodable: [plane] => (plane is Nominative, Tactic[Name.Error])
    =>  Name[plane] is Decodable in Text =

      decoder[plane](apply)

    private def decoder[plane](lambda: Text -> Name[plane]): Name[plane] is Decodable in Text =
      new Decodable:
        type Self = Name[plane]
        type Form = Text
        def decoded(text: Text): Name[plane] = lambda(text)

    inline def verify[NameType <: Label, plane] =
      ${protointernal.parse[plane, NameType]}

    transparent inline def apply[plane](name: Text): Name[plane] =
      ${protointernal.makeName[plane]('name)}

    // NameError → Name.Error
    case class Error(name: Text, rule: Rule, parameter: Text)(using Diagnostics)
    extends fulminate.Error(79, 0)
      ( m"the name $name is not valid because it ${rule.describe(parameter)}" )

    // NameExtractor → Name.Extractor
    class Extractor[text <: Label]():
      transparent inline def apply(): Any = ${protointernal.inferName[text]}

      inline def unapply[plane](inline scrutinee: Name[plane]): Boolean =
        ${protointernal.parse2[plane, text]('scrutinee)}
