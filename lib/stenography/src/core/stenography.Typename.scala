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
package stenography

import java.nio.charset as jnc

import anticipation.*
import denominative.*
import rudiments.*
import symbolism.*
import vacuous.*

object Typename:
  def fromUrl(text: Text): Typename = apply(text.s.replaceAll("~", "#").nn.tt)

  def apply(text: Text): Typename =
    def recur(i: Ordinal, start: Ordinal, typename: Optional[Typename]): Typename =
      def next = text.segment(start thru i - 1)

      text.at(i) match
        case Unset => typename.lay(Typename.Top(next))(Typename.Term(_, next))
        case '.'   => recur(i + 1, i + 1, typename.lay(Typename.Top(next))(Typename.Term(_, next)))
        case '#'   => recur(i + 1, i + 1, typename.lay(Typename.Top(next))(Typename.Type(_, next)))
        case char  => recur(i + 1, start, typename)

    recur(Prim, Prim, Unset)


enum Typename:
  case Top(name: Text)
  case Term(parent0: Typename, name: Text)
  case Type(parent0: Typename, name: Text)

  def name: Text
  def child(name: Text, isType: Boolean) = if isType then Type(this, name) else Term(this, name)

  def companionObject: Typename = this match
    case Type(parent, name) => Term(parent, name)
    case other              => other

  def companionType: Typename = this match
    case Term(parent, name) => Type(parent, name)
    case other              => other

  def parent: Optional[Typename] = this match
    case Type(parent, _) => parent
    case Term(parent, _) => parent
    case Top(_)          => Unset

  def symbol(typeSymbol: Text = "#", termSymbol: Text = "."): Text = parent match
    case Unset      => ""
    case Type(_, _) => typeSymbol
    case Term(_, _) => termSymbol
    case Top(_)     => termSymbol

  def render: Text = this match
    case Top(name)          => name
    case Type(parent, name) => s"${parent.render}${symbol("⌗")}$name".tt
    case Term(parent, name) => s"${parent.render}${symbol("⌗")}$name".tt

  def url: Text =
    val name2 = java.net.URLEncoder.encode(name.s, jnc.StandardCharsets.UTF_8).nn.tt

    this match
      case Top(_)          => name2
      case Type(parent, _) => s"${parent.url}${symbol("~")}$name2".tt
      case Term(parent, _) => s"${parent.url}${symbol("~")}$name2".tt

  def text(using imports: Imports): Text = parent.lay(name): parent =>
    if imports.has(parent) then name else s"${parent.text}${symbol("#")}$name".tt
