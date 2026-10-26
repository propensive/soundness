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
package stenography

import java.nio.charset as jnc

import anticipation.*
import denominative.*
import rudiments.*
import symbolism.*
import vacuous.*

object Designator:
  // Every designator made through `top`, `term` and `member` (and so through `apply`, `child`,
  // `companionObject` and `companionType`) is interned, so that equal designators are usually
  // the same instance and compare by reference. Rendering a name looks designators up in the
  // sets of an `Imports` many times over, and comparing nested designators structurally was
  // prominent in the cost of explaining a missing given (#2318). The table holds its entries
  // weakly, so it keeps no designator alive.
  private val interned: java.util.WeakHashMap[Designator, java.lang.ref.WeakReference[Designator]] =
    java.util.WeakHashMap()

  private def intern(designator: Designator): Designator = interned.synchronized:
    val existing = interned.get(designator)
    val instance = if existing == null then null else existing.get()

    if instance != null then instance else
      interned.put(designator, java.lang.ref.WeakReference(designator))
      designator

  def top(name: Text): Designator = intern(Top(name))
  def term(parent: Designator, name: Text): Designator = intern(Term(parent, name))
  def member(parent: Designator, name: Text): Designator = intern(Type(parent, name))

  def apply(text: Text): Designator =
    def recur(i: Ordinal, start: Ordinal, designator: Optional[Designator]): Designator =
      def next = text.segment(start thru i - 1)

      text(i) match
        case Unset => designator.lay(top(next))(term(_, next))
        case '.'   => recur(i + 1, i + 1, designator.lay(top(next))(term(_, next)))
        case '#'   => recur(i + 1, i + 1, designator.lay(top(next))(member(_, next)))
        case char  => recur(i + 1, start, designator)

    recur(Prim, Prim, Unset)

enum Designator:
  case Top(name: Text)
  case Term(parent0: Designator, name: Text)
  case Type(parent0: Designator, name: Text)

  // The hash is computed once, and equality tries the reference and the hash before the
  // structure, which is exact for a designator made with the case constructors rather than
  // interned, and fast for one which was.
  override lazy val hashCode: Int = this match
    case Top(name)          => name.hashCode
    case Term(parent, name) => (parent.hashCode*31 + name.hashCode)*31 + 1
    case Type(parent, name) => (parent.hashCode*31 + name.hashCode)*31 + 2

  override def equals(that: Any): Boolean = that match
    case that: Designator =>
      (this eq that) || hashCode == that.hashCode && ((this, that) match
        case (Top(name), Top(name2))                   => name == name2
        case (Term(parent, name), Term(parent2, name2)) => name == name2 && parent == parent2
        case (Type(parent, name), Type(parent2, name2)) => name == name2 && parent == parent2
        case _                                         => false)

    case _ =>
      false

  def name: Text

  def child(name: Text, isType: Boolean) =
    if isType then Designator.member(this, name) else Designator.term(this, name)

  def qualified: Text = text(using Imports.empty)

  def companionObject: Designator = this match
    case Type(parent, name) => Designator.term(parent, name)
    case other              => other

  def companionType: Designator = this match
    case Term(parent, name) => Designator.member(parent, name)
    case other              => other

  def parent: Optional[Designator] = this match
    case Type(parent, _) => parent
    case Term(parent, _) => parent
    case Top(_)          => Unset

  def symbol(typeSymbol: Text = "#", termSymbol: Text = "."): Text = parent match
    case Type(_, _) => typeSymbol
    case Term(_, _) => termSymbol
    case Top(_)     => termSymbol
    case _          => ""

  def render: Text = this match
    case Top(name)          => name
    case Type(parent, name) => s"${parent.render}${symbol("⌗")}$name".tt
    case Term(parent, name) => s"${parent.render}${symbol("⌗")}$name".tt

  def text(using imports: Imports): Text =
    if imports.hasDirect(this) then name else parent.lay(name): parent =>
      if imports.has(parent) then name else s"${parent.text}${symbol("#")}$name".tt
