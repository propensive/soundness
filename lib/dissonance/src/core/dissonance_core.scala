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
package dissonance

import scala.collection.immutable as sci

import anticipation.*
import denominative.*
import fulminate.*
import symbolism.*
import vacuous.*
import rudiments.*


def evolve[element: ClassTag]
  ( versions: List[List[element]], similar: Optional[(element, element) => Boolean] = Unset )
:   Evolution[element] =

  import Evolution.Atom


  def recur(iteration: Ordinal, todo: List[List[element]], evolution: Evolution[element])
  :   Evolution[element] =

    todo match
      case Nil | _ :: Nil => evolution

      case left :: right :: more =>
        val changes: List[Change[element]] =
          val diff0 = diff(Series.from(left.stdlib), Series.from(right.stdlib))
          similar.lay(diff0.edits)(diff0.rdiff(_).changes).to(List)


        def merge
          ( atoms:   List[Atom[element]],
            edits:   List[Change[element]],
            done:    List[Atom[element]]   = Nil,
            skips:   List[Atom[element]]   = Nil,
            inserts: List[Atom[element]]   = Nil )
        :   List[Atom[element]] =

          def finish(): List[Atom[element]] =
            val left = IArray.from(skips.stdlib)
            val right = IArray.from(inserts.stdlib)

            val updates =
              diff(Series.from(left.stdlib), Series.from(right.stdlib), _.value == _.value).edits.toList.map:
                case Ins(_, value)    => value
                case Del(index, _)    => left.stdlib(index)
                case Par(index, _, _) => left.stdlib(index).add(iteration)

            List.of(updates ::: done.stdlib)

          edits match
            case Nil => atoms match
              case Nil           => List.of(finish().stdlib.reverse)
              case atom :: atoms => merge(atoms, Nil, done, atom :: skips, inserts)

            case edit :: edits => atoms match
              case Nil => edit match
                case Ins(_, value) =>
                  merge(Nil, edits, Atom(value, Set(iteration)) :: finish())

                case edit =>
                  panic(m"Unexpected edit: ${edit.toString}")

              case atom :: atoms =>
                if !atom.has(iteration - 1)
                then merge(atoms, edit :: (edits: List[Change[element]]), done, atom :: skips, inserts)
                else edit match
                  case Ins(_, value) =>
                    val atom2 = Atom(value, Set(iteration))
                    merge(atom :: (atoms: List[Atom[element]]), edits, done, skips, atom2 :: inserts)

                  case Del(_, value) =>
                    merge(atoms, edits, done, atom :: skips, inserts)

                  case Par(_, _, value) =>
                    merge(atoms, edits, atom.add(iteration) :: finish())

                  case Sub(_, _, _, _) =>
                    merge(atoms, edits, atom.add(iteration) :: finish())


        recur
          ( iteration + 1, right :: (more: List[List[element]]),
            Evolution(merge(evolution.sequence, changes)) )


  if versions.nil then Evolution(Nil)
  else
    recur
      ( Sec, versions,
        Evolution(List.of(versions.stdlib.head.stdlib.map(Atom(_, Set(Prim))))) )

def diff[element]
  ( leftSeries:  Series[element],
    rightSeries: Series[element],
    compare:     (element, element) => Boolean = { (a: element, b: element) => a == b } )
:   Diff[element] =

  val left = leftSeries.stdlib
  val right = rightSeries.stdlib

  type Edits = List[Edit[element]]

  @tailrec
  def count(position: Int, offset: Int): Int =
    if
      position < left.length && position + offset < right.length &&
        compare(left(position), right(position + offset))
    then count(position + 1, offset)
    else position

  @tailrec
  def trace(deletes: Int, inserts: Int, focus: sci.List[Int], rows: sci.List[Array[Int]])
  :   Diff[element] =

    val delPos = if deletes == 0 then 0 else count(rows.head(deletes - 1) + 1, inserts - deletes)
    val insPos = if inserts == 0 then 0 else count(rows.head(deletes), inserts - deletes)
    val best = if deletes + inserts == 0 then count(0, 0) else delPos.max(insPos)

    if best == left.length && (best - deletes + inserts) == right.length
    then Diff(backtrack(left.length - 1, deletes, rows, Nil)*)
    else if inserts > 0 then trace(deletes + 1, inserts - 1, best :: focus, rows)
    else trace(0, deletes + 1, sci.Nil, ((best :: focus).reverse).to(Array) :: rows)

  @tailrec
  def backtrack(position: Int, deletes: Int, rows: sci.List[Array[Int]], edits: Edits): Edits =
    val rightPosition = position + rows.length - deletes*2
    lazy val ins = rows.head(deletes) - 1
    lazy val del = rows.head(deletes - 1)

    if position == -1 && rightPosition == -1 then edits else if rows.isEmpty
    then
      backtrack
        ( position - 1, deletes, rows,
          List.of(Par(position, rightPosition, left(position)) :: edits.stdlib) )

    else if deletes < rows.length && (deletes == 0 || ins >= del)
    then
      if position == ins
      then
        backtrack
          ( position, deletes, rows.tail,
            List.of(Ins(rightPosition, right(rightPosition)) :: edits.stdlib) )
      else
        backtrack
          ( position - 1, deletes, rows,
          List.of(Par(position, rightPosition, left(position)) :: edits.stdlib) )
    else
      if position == del
      then
        backtrack
          ( del - 1, deletes - 1, rows.tail,
            List.of(Del(position, left(position)) :: edits.stdlib) )
      else
        backtrack
          ( position - 1, deletes, rows,
          List.of(Par(position, rightPosition, left(position)) :: edits.stdlib) )

  trace(0, 0, sci.Nil, sci.Nil)
