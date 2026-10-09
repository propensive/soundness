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
package probably

import anticipation.*
import beneficence.*
import digression.*
import fulminate.*
import nomenclature.*
import prepositional.*
import rudiments.*
import vacuous.*

// What tests are attached to: a `Suite`, a `suite` block within one, or an `impromptu` block.
// Its `Topic` says, statically, where its tests belong: the literal id of the `Suite` they are
// declared for (`Testable of "json"`), which every `suite` block within it passes on; `Derived`
// for a suite whose id is derived from its title; or `Impromptu` for tests that are only known
// by running them. A declaration (`test`, `suite`,
// sedentary's `bench`) demands a `Testable` whose topic is one or the other, so the compiler
// refuses a test that belongs nowhere — and the beneficence plugin, reading the topic from each
// declaration it compiles, can list every suite's tests without running anything.
class Testable private[probably]
  ( val name:    Message,
        parents: List[Testable],
    val moniker: Optional[Name[Probing]],
    val key:     Optional[Text] = Unset )
  ( using codepoint: Codepoint )
extends Findable:

  type Topic

  // The group this one is within, if it is not a root. (Held as a list because a constructor
  // parameter typed as a union with `Testable` itself, a class with an abstract type member,
  // sends the compiler into a cycle.)
  def parent: Optional[Testable] = parents.prim

  override def equals(that: Any): Boolean = that.unsafeMatchable(using Unsafe) match
    case that: Testable => name == that.name && parent == that.parent
    case _              => false

  // By the text of the names alone: `Test.Id#id` is specified in terms of it.
  override def hashCode: Int = name.text.s.hashCode + parent.lay(0)(_.hashCode)

  val id: Test.Id = Test.Id(name, parent, codepoint, moniker, Nil, key)

object Testable:
  // What the compiler says of a declaration with no `Testable` that has a topic. On each
  // declaration's parameter rather than on the class, where it would not be consulted for
  // the refined type that is sought.
  final val orphan =
    "a test must be declared for a Suite — in its body, or in a method taking `(using Testable " +
    "of \"<the suite's name>\")` — or inside an `impromptu` block"

  def of[topic]
    ( name:    Message,
      parent:  Optional[Testable]      = Unset,
      moniker: Optional[Name[Probing]] = Unset,
      key:     Optional[Text]          = Unset )
    ( using Codepoint )
  :   Testable of topic =

    new Testable(name, parent.lay(Nil)(List(_)), moniker, key) { type Topic = topic }

  // The same position in the hierarchy as `testable` (equality is structural), for tests
  // which are not statically attributed to it.
  def impromptu(testable: Testable): Testable of Impromptu =
    of[Impromptu](testable.name, testable.parent, testable.moniker, testable.key)
      ( using testable.id.codepoint )

  // Where `impromptu` tests go when no `Testable` surrounds the block.
  val detached: Testable of Impromptu = of[Impromptu](m"impromptu")

// The topic of a `Testable` whose tests are not attributed to a `Suite` at compile time: they
// are declared as the program runs, inside an `impromptu` block, and no listing made without
// running the suite includes them. It is a `Label`, as a suite's name is, but an abstract one,
// which no literal equals.
opaque type Impromptu <: Label = "impromptu"

// The topic of a suite whose id is derived from its title, `Suite(m"Jacinta tests")`: a `Label`
// which no literal equals, like `Impromptu`, so such a suite's tests can only be declared
// where its own `Testable` is in scope — its body — or in a method which is generic in the
// topic. A suite given an id has that id for its topic.
opaque type Derived <: Label = "derived"
