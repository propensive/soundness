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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package digression

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import vacuous.*

object Fqcn:
  def valid(char: Char): Boolean =
    char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z' || char >= '0' && char <= '9' ||
      char == '_' || char == '$'

  def apply(name: Text): Fqcn raises FqcnError =
    val parts = scala.IArray.from(name.s.split("\\.").nn.iterator.map(_.nn))

    parts.each: part =>
      if part.length == 0 then raise(FqcnError(name, FqcnError.Reason.EmptyName))

      if digression.internal.javaKeywords.has(part)
      then raise(FqcnError(name, FqcnError.Reason.JavaKeyword(part.tt)))

      part.foreach: char =>
        if !valid(char) then raise(FqcnError(name, FqcnError.Reason.InvalidChar(char)))

      if part.head >= '0' && part.head <= '9'
      then raise(FqcnError(name, FqcnError.Reason.InvalidStart(part.head)))

    new Fqcn(parts.map(_.tt))

  given encodable: Fqcn is Encodable in Text = _.text

  private[digression] def join(parts: IArray[Text], count: Int): Text =
    val builder = StringBuilder()

    var index = 0
    while index < count do
      if index > 0 then builder.append(".")
      builder.append(parts(index).s)
      index += 1

    builder.toString.tt

class Fqcn(val parts: IArray[Text]):
  def text: Text = Fqcn.join(parts, parts.length)
  def className: Text = parts.last
  def packageName: Text = Fqcn.join(parts, parts.length - 1)
