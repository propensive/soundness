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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import language.experimental.captureChecking

import denominative.*
import fulminate.*
import proscenium.*
import rudiments.*
import symbolism.*

case class Evolution[element](sequence: List[Evolution.Atom[element]]):
  def apply(version: Ordinal): List[element] =
    sequence.filter(_.has(version)).map(_.value)

object Evolution:
  case class Atom[element](value: element, presence: Set[Ordinal]):
    def add(n: Ordinal): Atom[element] = copy(presence = presence + n)
    def has(n: Ordinal): Boolean = presence.contains(n)

  def of[element](versions: List[List[element]]): Evolution[element] =
    def recur(iteration: Ordinal, todo: List[Seq[element]], evolution: Evolution[element])
    :     Evolution[element] =
      todo match
        case Nil | _ :: Nil => evolution

        case left :: right :: more =>
          val changes: List[Edit[element]] = diff(left.to(Vector), right.to(Vector)).edits.to(List)

          def merge
               (atoms: List[Atom[element]], edits: List[Edit[element]], done: List[Atom[element]])
          :     List[Atom[element]] =

            edits match
              case Nil           => done.unwind(atoms)
              case edit :: edits => atoms match
                case Nil => edit match
                  case Ins(_, value) =>
                    merge(Nil, edits, Atom(value, Set(iteration)) :: done)
                  case edit          => panic(m"Unexpected edit: ${edit.toString}")

                case atom :: atoms =>
                  if !atom.has(iteration - 1) then merge(atoms, edit :: edits, atom :: done)
                  else edit match
                    case Ins(_, value) =>
                      merge(atom :: atoms, edits, Atom(value, Set(iteration)) :: done)

                    case Del(_, value) =>
                      if atom.value != value then panic(m"Expected value for deletion")
                      merge(atoms, edits, atom :: done)

                    case Par(_, _, value) =>
                      if atom.value != value then panic(m"Expected parity value")
                      merge(atoms, edits, atom.add(iteration) :: done)

          recur(iteration + 1, right :: more, Evolution(merge(evolution.sequence, changes, Nil)))

    if versions.isEmpty then Evolution(Nil) else
      val initial = Evolution(versions.head.map(Atom(_, Set(Prim))))
      recur(Sec, versions, initial)
