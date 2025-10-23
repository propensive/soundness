                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                               ╭───╮                              ┃
┃                                                               │   │                              ┃
┃                                                               │   │                              ┃
┃                     ╭─────────╮ ╭───╮╌────╮╌────╮ ╭─────────╮ │   │ ╭───╮                        ┃
┃                     ╰─────╮   │ │   ╭─╮   ╭─╮   │ │   ╭─╮   │ │   │╌╯   │                        ┃
┃                     ╭─────╯   │ │   │ │   │ │   │ │   │ │   │ │        ╌╯                        ┃
┃                     │   ╭─╮   │ │   │ │   │ │   │ │   │ │   │ │   ╭─╮   │                        ┃
┃                     │   ╰─╯   │ │   │ │   │ │   │ │   ╰─╯   │ │   │ │   │                        ┃
┃                     ╰─────────╯ ╰───╯ ╰───╯ ╰───╯ ╰─────────╯ ╰───╯ ╰───╯                        ┃
┃                                                                                                  ┃
┃    Amok, prerelease version                                                                      ┃
┃    © Copyright 2023-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://github.com/propensive/amok/                                                       ┃
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

import soundness.*

object Typename:
  def apply(text: Text): Typename = text.where(_ == '.', bidi = Rtl).lay(Top(text)): position =>
    val next = text.after(position)
    if next.ends(t"package$$") then apply(text.before(position))
    else Term(apply(text.before(position)), text.after(position))

  def decode(text: Text): Typename =
    def entity(start: Ordinal, end: Ordinal, isType: Boolean, parent: Optional[Typename])
    : Typename =
        val part = text.segment(start thru end)
        parent.lay(Typename.Top(part)): name =>
          if isType then Typename.Type(name, part) else Typename.Term(name, part)


    def recur(position: Ordinal, start: Ordinal, isType: Boolean, typename: Optional[Typename])
    : Typename =
        text.at(position) match
          case Unset =>
            entity(start, position - 1, isType, typename)

          case char@('.' | ':') =>
            recur
             (position + 1,
              position + 1,
              char == ':',
              entity(start, position - 1, isType, typename))

          case char  =>
            recur(position + 1, start, isType, typename)

    recur(Prim, Prim, false, Unset)


  given (scope: Scope) => Typename is Showable =
    case Typename.Top(name)          => name
    case Typename.Term(parent, name) => if scope.has(parent) then name else parent.show+t"."+name
    case Typename.Type(parent, name) => if scope.has(parent) then name else parent.show+t"."+name

enum Typename:
  case Top(name: Text)
  case Term(parent0: Typename, name: Text)
  case Type(parent0: Typename, name: Text)

  def child(name: Text, isType: Boolean) = if isType then Type(this, name) else Term(this, name)

  def parent: Typename = this match
    case Type(parent, _) => parent
    case Term(parent, _) => parent
    case Top(_)          => this

  def id: Text = this match
    case Top(name)          => name
    case Term(parent, name) => t"${parent.id}.${name.urlEncode}"
    case Type(parent, name) => t"${parent.id}:${name.urlEncode}"

  def text: Text = this match
    case Top(name)          => name
    case Term(parent, name) => t"${parent.text}.$name"
    case Type(parent, name) => t"${parent.text}⌗$name"
