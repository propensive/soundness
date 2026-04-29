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
package dissonance

import anticipation.*
import denominative.*
import fulminate.*
import proscenium.*
import symbolism.*
import vacuous.*

def evolve[element: ClassTag]
  ( versions: List[List[element]], similar: Optional[(element, element) => Boolean] = Unset )
:   Evolution[element] =

  import Evolution.Atom


  def recur(iteration: Ordinal, todo: List[Seq[element]], evolution: Evolution[element])
  :   Evolution[element] =

    todo match
      case Nil | _ :: Nil => evolution

      case left :: right :: more =>
        val changes: List[Change[element]] =
          val diff0 = diff(IArray.from(left), IArray.from(right))
          similar.lay(diff0.edits)(diff0.rdiff(_).changes).to(List)


        def merge
          ( atoms:   List[Atom[element]],
            edits:   List[Change[element]],
            done:    List[Atom[element]]   = Nil,
            skips:   List[Atom[element]]   = Nil,
            inserts: List[Atom[element]]   = Nil )
        :   List[Atom[element]] =

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
                case Ins(_, value) =>
                  merge(Nil, edits, Atom(value, Set(iteration)) :: finish())

                case edit =>
                  panic(m"Unexpected edit: ${edit.toString}")

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
def diff[element]
  ( left:    IndexedSeq[element],
    right:   IndexedSeq[element],
    compare: (element, element) => Boolean = { (a: element, b: element) => a == b } )
:   Diff[element] =

  type Edits = List[Edit[element]]

  @tailrec
  def count(position: Int, offset: Int): Int =
    if
      position < left.length && position + offset < right.length
      && compare(left(position), right(position + offset))
    then count(position + 1, offset)
    else position

  @tailrec
  def trace(deletes: Int, inserts: Int, focus: List[Int], rows: List[Array[Int]]): Diff[element] =
    val delPos = if deletes == 0 then 0 else count(rows.head(deletes - 1) + 1, inserts - deletes)
    val insPos = if inserts == 0 then 0 else count(rows.head(deletes), inserts - deletes)
    val best = if deletes + inserts == 0 then count(0, 0) else delPos.max(insPos)

    if best == left.length && (best - deletes + inserts) == right.length
    then Diff(backtrack(left.length - 1, deletes, rows, Nil)*)
    else if inserts > 0 then trace(deletes + 1, inserts - 1, best :: focus, rows)
    else trace(0, deletes + 1, Nil, ((best :: focus).reverse).to(Array) :: rows)

  @tailrec
  def backtrack(position: Int, deletes: Int, rows: List[Array[Int]], edits: Edits): Edits =
    val rightPosition = position + rows.length - deletes*2
    lazy val ins = rows.head(deletes) - 1
    lazy val del = rows.head(deletes - 1)

    if position == -1 && rightPosition == -1 then edits else if rows.isEmpty
    then
      backtrack
        ( position - 1, deletes, rows, Par(position, rightPosition, left(position)) :: edits )

    else if deletes < rows.length && (deletes == 0 || ins >= del)
    then
      if position == ins
      then
        backtrack
          ( position, deletes, rows.tail, Ins(rightPosition, right(rightPosition)) :: edits )
      else
        backtrack
          ( position - 1, deletes, rows, Par(position, rightPosition, left(position)) :: edits )
    else
      if position == del
      then backtrack(del - 1, deletes - 1, rows.tail, Del(position, left(position)) :: edits)
      else
        backtrack
          ( position - 1, deletes, rows, Par(position, rightPosition, left(position)) :: edits )

  trace(0, 0, Nil, Nil)
