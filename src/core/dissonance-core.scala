/*
    Dissonance, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dissonance

import language.experimental.captureChecking

import anticipation.*
import proscenium.*

def diff[ElemType]
   (left:    IndexedSeq[ElemType],
    right:   IndexedSeq[ElemType],
    compare: (ElemType, ElemType) -> Boolean = { (a: ElemType, b: ElemType) => a == b })
:     Diff[ElemType] =

  type Edits = List[Edit[ElemType]]

  @tailrec
  def count(pos: Int, off: Int): Int =
    if pos < left.length && pos + off < right.length && compare(left(pos), right(pos + off))
    then count(pos + 1, off)
    else pos

  @tailrec
  def trace(deletes: Int, inserts: Int, focus: List[Int], rows: List[Array[Int]]): Diff[ElemType] =
    val delPos = if deletes == 0 then 0 else count(rows.head(deletes - 1) + 1, inserts - deletes)
    val insPos = if inserts == 0 then 0 else count(rows.head(deletes), inserts - deletes)
    val best = if deletes + inserts == 0 then count(0, 0) else delPos.max(insPos)

    if best == left.length && (best - deletes + inserts) == right.length
    then Diff(backtrack(left.length - 1, deletes, rows, Nil)*)
    else if inserts > 0 then trace(deletes + 1, inserts - 1, best :: focus, rows)
    else trace(0, deletes + 1, Nil, ((best :: focus).reverse).to(Array) :: rows)

  @tailrec
  def backtrack(pos: Int, deletes: Int, rows: List[Array[Int]], edits: Edits): Edits =
    val rpos = pos + rows.length - deletes*2
    lazy val ins = rows.head(deletes) - 1
    lazy val del = rows.head(deletes - 1)

    if pos == -1 && rpos == -1 then edits else if rows.isEmpty
    then backtrack(pos - 1, deletes, rows, Par(pos, rpos, left(pos)) :: edits)
    else if deletes < rows.length && (deletes == 0 || ins >= del)
    then
      if pos == ins then backtrack(pos, deletes, rows.tail, Ins(rpos, right(rpos)) :: edits)
      else backtrack(pos - 1, deletes, rows, Par(pos, rpos, left(pos)) :: edits)
    else
      if pos == del then backtrack(del - 1, deletes - 1, rows.tail, Del(pos, left(pos)) :: edits)
      else backtrack(pos - 1, deletes, rows, Par(pos, rpos, left(pos)) :: edits)

  trace(0, 0, Nil, Nil)

extension (diff: Diff[Text])
  def serialize: Stream[Text] = diff.chunks.flatMap:
    case Chunk(left, right, dels, inss) =>
      def range(start: Int, end: Int): Text = s"$start${if start == end then "" else s",$end"}".tt

      val command: Text =
        if dels.isEmpty then s"${left}a${range(right + 1, right + inss.size)}".tt
        else if inss.isEmpty then s"${range(left + 1, left + dels.size)}d${right}".tt
        else s"${range(left + 1, left + dels.size)}c${range(right + 1, right + inss.size)}".tt

      val delSeq = dels.map { del => Text("< "+del.value) }
      val sep = if inss.size > 0 && dels.size > 0 then List(Text("---")) else List()
      val insSeq = inss.map { ins => Text("> "+ins.value) }

      command :: delSeq ::: sep ::: insSeq

  /* def casual: CasualDiff =
    def recur(todo: List[Region[Text]], acc: List[Replace], last: List[Text]): CasualDiff =
      todo match
        case Nil                            => CasualDiff(acc.reverse)
        case Region.Unchanged(pars) :: tail => recur(tail, acc, pars.map(_.value).compact)

        case Region.Changed(deletions, insertions) :: tail =>
          val deletions2 = deletions.map(_.value).compact
          val prelude = (last ++ deletions2).to(Array)

          def replace(deletions: List[Text], target: Int): Replace =
            val index = prelude.indexOfSlice(deletions)
            if index == target
            then
              Replace(deletions.dropRight(deletions2.length), deletions2, insertions.map(_.value))
            else
              def countback(n: Int): Int =
                if index - n > 0 && prelude(target - n) == prelude(index - n) then countback(n + 1)
                else n

              val n = countback(1)
              replace(prelude.slice(target - n, target).to(List) ++ deletions, target - n)

          recur(tail, replace(deletions2, last.length) :: acc, Nil)

    recur(diff.collate, Nil, Nil) */
