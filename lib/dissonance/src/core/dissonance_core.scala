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

import anticipation.*
import denominative.*
import fulminate.*
import symbolism.*
import vacuous.*
import rudiments.*
import denominative.dysasymptotics.linearSize


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
          val diff0 = diff(left.to[Sequence], right.to[Sequence])
          similar.lay(diff0.edits)(diff0.rdiff(_).changes).to(List)


        def merge
          ( atoms:   List[Atom[element]],
            edits:   List[Change[element]],
            done:    List[Atom[element]]   = Nil,
            skips:   List[Atom[element]]   = Nil,
            inserts: List[Atom[element]]   = Nil )
        :   List[Atom[element]] =

          def finish(): List[Atom[element]] =
            val left = skips.to[Array]
            val right = inserts.to[Array]

            val updates: List[Atom[element]] =
              diff(Sequence.from(left.readable), Sequence.from(right.readable), _.value == _.value)
              . edits.to(List).map:
                  case Ins(_, value)    => value
                  case Del(index, _)    => left.readable(index)
                  case Par(index, _, _) => left.readable(index).add(iteration)

            updates + done

          edits match
            case Nil => atoms match
              case Nil           => finish().reverse
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


  versions match
    case Nil          => Evolution(Nil)
    case first :: _   => recur(Sec, versions, Evolution(first.map(Atom(_, Set(Prim)))))

def diff[element]
  ( leftSeries:  Sequence[element],
    rightSeries: Sequence[element],
    compare:     (element, element) => Boolean = { (a: element, b: element) => a == b } )
:   Diff[element] & Retained =

  // The Myers walk reads both sequences positionally, thousands of times, at indices its own
  // invariants already keep in range; a single stdlib view of each is bound here rather than
  // threading a bounds proof through every read.
  val left = leftSeries.stdlib
  val right = rightSeries.stdlib

  // Every edit the backtrack constructs carries its element (`left(position)` or
  // `right(rightPosition)`), so each is minted `Retained` at construction and the assembled
  // `Diff` carries the proof.
  type Edits = List[Edit[element] & Retained]

  @tailrec
  def count(position: Int, offset: Int): Int =
    if
      position < left.length && position + offset < right.length &&
        compare(left(position), right(position + offset))
    then count(position + 1, offset)
    else position

  @tailrec
  def trace(deletes: Int, inserts: Int, focus: List[Int], rows: List[Array[Int]^{}])
  :   Diff[element] & Retained =

    // The row stack is popped and indexed by the walk's own invariants, so one stdlib view
    // serves every read; it is lazy because an empty stack is legal on the first step.
    lazy val row = rows.stdlib.head

    val delPos = if deletes == 0 then 0 else count(row.readable(deletes - 1) + 1, inserts - deletes)
    val insPos = if inserts == 0 then 0 else count(row.readable(deletes), inserts - deletes)
    val best = if deletes + inserts == 0 then count(0, 0) else delPos.max(insPos)

    if best == left.length && (best - deletes + inserts) == right.length
    then Diff(backtrack(left.length - 1, deletes, rows, Nil)*).retained
    else if inserts > 0 then trace(deletes + 1, inserts - 1, best :: focus, rows)
    else trace(0, deletes + 1, Nil, (focus.reverse + List(best)).to[Array] :: rows)

  @tailrec
  def backtrack(position: Int, deletes: Int, rows: List[Array[Int]^{}], edits: Edits): Edits =
    val rightPosition = position + rows.size - deletes*2

    // As in `trace`, one lazy stdlib view of the row stack serves the head reads and the pop,
    // both of which the walk's invariants keep legal.
    lazy val rowStack = rows.stdlib
    lazy val ins = rowStack.head.readable(deletes) - 1
    lazy val del = rowStack.head.readable(deletes - 1)

    if position == -1 && rightPosition == -1 then edits else if rows.nil
    then
      backtrack
        ( position - 1, deletes, rows,
          Par(position, rightPosition, left(position)).retained :: edits )

    else if deletes < rows.size && (deletes == 0 || ins >= del)
    then
      if position == ins
      then
        backtrack
          ( position, deletes, rowStack.tail.to(List),
            Ins(rightPosition, right(rightPosition)).retained :: edits )
      else
        backtrack
          ( position - 1, deletes, rows,
            Par(position, rightPosition, left(position)).retained :: edits )
    else
      if position == del
      then
        backtrack
          ( del - 1, deletes - 1, rowStack.tail.to(List),
            Del(position, left(position)).retained :: edits )
      else
        backtrack
          ( position - 1, deletes, rows,
            Par(position, rightPosition, left(position)).retained :: edits )

  trace(0, 0, Nil, Nil)
