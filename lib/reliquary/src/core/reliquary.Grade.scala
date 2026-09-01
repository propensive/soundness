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
package reliquary

import anticipation.*
import rudiments.*

object Grade:
  // Each intermediate is annotated so one combinator's inferred result type is pinned before the
  // next one's implicit search runs over it; an uninstantiated shape there trips `wildApprox`.
  private def entries(atomizations: List[Atomization]): Set[(Text, Atom.Class, Text)] =
    val atoms: List[Atom] = atomizations.flatMap(_.atoms)

    val triples: List[(Text, Atom.Class, Text)] = atoms.map: atom =>
      (atom.key, atom.atomClass, Lira.Hash.text(atom.valueHash))

    triples.to[Set]

  // §12.3: the grade of a successor release relative to its predecessor. `Patch` is API
  // identity; `Minor` is pure rigid extension plus replaceable churn (every replaceable key of
  // the predecessor survives, unchanged or replaced); anything else is `Major` and must begin a
  // fresh lineage.
  def between(previous: List[Atomization], next: List[Atomization]): Grade =
    val before = entries(previous)
    val after = entries(next)

    if before == after then Patch
    else
      // `subsetOf` has no native counterpart; `all` over the superset's `has` is the same test.
      type Entry = (Text, Atom.Class, Text)
      val rigidBefore: Set[Entry] = before.filter(_(1) == Atom.Class.Rigid)
      val rigidAfter: Set[Entry] = after.filter(_(1) == Atom.Class.Rigid)
      val rigidKept = rigidBefore.all: entry => rigidAfter.has(entry)

      val replaceableBefore: Set[Entry] = before.filter(_(1) == Atom.Class.Replaceable)
      val replaceableAfter: Set[Entry] = after.filter(_(1) == Atom.Class.Replaceable)
      val keysBefore: Set[Text] = replaceableBefore.map(_(0))
      val keysAfter: Set[Text] = replaceableAfter.map(_(0))
      val keysKept = keysBefore.all: key => keysAfter.has(key)

      if rigidKept && keysKept then Minor else Major

enum Grade:
  case Patch, Minor, Major
