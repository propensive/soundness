/*
    Dissonance, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import rudiments.*
import eucalyptus.*

import language.experimental.captureChecking

sealed trait Change[+ElemType] extends Product

sealed trait Edit[+ElemType] extends Change[ElemType]:
  def value: ElemType

case class Ins[+ElemType](right: Int, value: ElemType) extends Edit[ElemType]
case class Del[+ElemType](left: Int, value: ElemType) extends Edit[ElemType]
case class Par[+ElemType](left: Int, right: Int, value: ElemType) extends Edit[ElemType]
 
case class Sub[+ElemType](left: Int, right: Int, leftValue: ElemType, rightValue: ElemType)
extends Change[ElemType]

enum Region[ElemType]:
  case Changed(deletions: List[Del[ElemType]], insertions: List[Ins[ElemType]])
  case Unchanged(retentions: List[Par[ElemType]])

enum Chunk[+ElemType]:
  case Ins(startRight: Int, values: List[ElemType])
  case Del(startLeft: Int, values: List[ElemType])
  case Par(startLeft: Int, startRight: Int, values: List[ElemType])
  case Sub(startLeft: Int, startRight: Int, valuesLeft: List[ElemType], valuesRight: List[ElemType])

case class RDiff[ElemType](changes: Change[ElemType]*):
  def flip: RDiff[ElemType] =
    val changes2 = changes.map:
      case Par(left, right, value)                 => Par(right, left, value)
      case Del(left, value)                        => Ins(left, value)
      case Ins(right, value)                       => Del(right, value)
      case Sub(left, right, leftValue, rightValue) => Sub(right, left, rightValue, leftValue)
    
    RDiff[ElemType](changes2*)

case class Diff[ElemType](edits: Edit[ElemType]*):
  def flip: Diff[ElemType] =
    val edits2 = edits.map:
      case Par(left, right, value) => Par(right, left, value)
      case Del(left, value)        => Ins(left, value)
      case Ins(right, value)       => Del(right, value)
    
    Diff[ElemType](edits2*)
  
  def apply(list: List[ElemType], update: (ElemType, ElemType) -> ElemType): LazyList[ElemType] =
    def recur(todo: List[Edit[ElemType]], list: List[ElemType]): LazyList[ElemType] = todo match
      case Ins(_, value) :: tail    => value #:: recur(tail, list)
      case Del(_, _) :: tail        => recur(tail, list.tail)
      case Par(_, _, value) :: tail => update(value, list.head) #:: recur(tail, list.tail)
      case Nil                      => LazyList()

    recur(edits.to(List), list)

  def rdiff(similar: (ElemType, ElemType) -> Boolean, subSize: Int = 1): RDiff[ElemType] =
    val changes = collate.flatMap:
      case Region.Unchanged(pars)    => pars
      case Region.Changed(dels, Nil) => dels
      case Region.Changed(Nil, inss) => inss
      
      case Region.Changed(dels, inss) =>
        if inss.length == dels.length && inss.length <= subSize
        then dels.zip(inss).map { (del, ins) => Sub(del.left, ins.right, del.value, ins.value) }
        else
          val delsSeq = dels.map(_.value).to(IndexedSeq)
          val inssSeq = inss.map(_.value).to(IndexedSeq)
          
          diff(delsSeq, inssSeq, similar).edits.map:
            case Del(l, v)    => Del(dels(l).left, dels(l).value)
            case Ins(r, v)    => Ins(inss(r).right, inss(r).value)
            case Par(l, r, _) => Sub(dels(l).left, inss(r).right, dels(l).value, inss(r).value)
    
    RDiff(changes*)
          
  def collate: List[Region[ElemType]] =
    edits.runs:
      case Par(_, _, _) => true
      case _            => false
    .map:
      case xs@(Par(_, _, _) :: _) => Region.Unchanged(xs.sift[Par[ElemType]])
      case xs                     => Region.Changed(xs.sift[Del[ElemType]], xs.sift[Ins[ElemType]])
  
def diff
    [ElemType]
    (left: IndexedSeq[ElemType], right: IndexedSeq[ElemType],
        compare: (ElemType, ElemType) -> Boolean = { (a: ElemType, b: ElemType) => a == b })
    : Diff[ElemType] =
  @tailrec
  def count(pos: Int, off: Int): Int =
    if pos < left.length && pos + off < right.length && compare(left(pos), right(pos + off))
    then count(pos + 1, off)
    else pos

  @tailrec
  def trace
      (deletes: Int = 0, inserts: Int = 0, current: List[Int] = Nil, rows: List[IArray[Int]] = Nil)
      : Diff[ElemType] =
    val delPos = if deletes == 0 then 0 else count(rows.head(deletes - 1) + 1, inserts - deletes)
    val insPos = if inserts == 0 then 0 else count(rows.head(deletes), inserts - deletes)
    val best = if deletes + inserts == 0 then count(0, 0) else delPos.max(insPos)
    
    if best == left.length && (best - deletes + inserts) == right.length
    then Diff(backtrack(left.length - 1, deletes, rows, Nil)*)
    else if inserts > 0 then trace(deletes + 1, inserts - 1, best :: current, rows)
    else trace(0, deletes + 1, Nil, IArray.from((best :: current).reverse) :: rows)

  @tailrec
  def backtrack
      (pos: Int, deletes: Int, rows: List[IArray[Int]], edits: List[Edit[ElemType]])
      : List[Edit[ElemType]] =
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
    
  trace()

given Realm = Realm(t"dissonance")
