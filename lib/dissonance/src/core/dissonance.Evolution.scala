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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package dissonance

import contingency.*
import denominative.*
import fulminate.*
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

case class Evolution[element](sequence: List[Evolution.Atom[element]]):
  def apply(version: Ordinal): List[element] =
    sequence.filter(_.has(version)).map(_.value)

object Evolution:
  case class Atom[element](value: element, presence: Set[Ordinal]):
    def add(n: Ordinal): Atom[element] = copy(presence = presence + n)
    def has(n: Ordinal): Boolean = presence.contains(n)

def evolve[element: ClassTag]
     (versions: List[List[element]], similar: Optional[(element, element) => Boolean] = Unset)
: Evolution[element] =

    import Evolution.Atom


    def recur(iteration: Ordinal, todo: List[Seq[element]], evolution: Evolution[element])
    : Evolution[element] =

        todo match
          case Nil | _ :: Nil => evolution

          case left :: right :: more =>
            val changes: List[Change[element]] =
                val diff0 = diff(IArray.from(left), IArray.from(right))
                similar.lay(diff0.edits)(diff0.rdiff(_).changes).to(List)


            def merge
                 (atoms:   List[Atom[element]],
                  edits:   List[Change[element]],
                  done:    List[Atom[element]] = Nil,
                  skips:   List[Atom[element]] = Nil,
                  inserts: List[Atom[element]] = Nil)
            : List[Atom[element]] =

                def finish(): List[Atom[element]] =
                  val left = IArray.from(skips)
                  val right = IArray.from(inserts)

                  val updates: List[Atom[element]] =
                    diff(left, right, _.value == _.value).edits.to(List).map:
                      case Ins(_, value)    => value
                      case Del(index, _)    => left(index)
                      case Par(index, _, _) => left(index).add(iteration)

                  updates ::: done

                edits match
                  case Nil => atoms match
                    case Nil           => finish().reverse
                    case atom :: atoms => merge(atoms, Nil, done, atom :: skips, inserts)

                  case edit :: edits => atoms match
                    case Nil => edit match
                      case Ins(_, value) => merge
                                             (Nil, edits, Atom(value, Set(iteration)) :: finish())
                      case edit          => panic(m"Unexpected edit: ${edit.toString}")

                    case atom :: atoms =>
                      if !atom.has(iteration - 1)
                      then merge(atoms, edit :: edits, done, atom :: skips, inserts)
                      else edit match
                        case Ins(_, value) =>
                          val atom2 = Atom(value, Set(iteration))
                          merge(atom :: atoms, edits, done, skips, atom2 :: inserts)

                        case Del(_, value) =>
                          merge(atoms, edits, done, atom :: skips, inserts)

                        case Par(_, _, value) =>
                          merge(atoms, edits, atom.add(iteration) :: finish())

                        case Sub(_, _, _, _) =>
                          merge(atoms, edits, atom.add(iteration) :: finish())


            recur(iteration + 1, right :: more, Evolution(merge(evolution.sequence, changes)))


    if versions.isEmpty then Evolution(Nil)
    else recur(Sec, versions, Evolution(versions.head.map(Atom(_, Set(Prim)))))
