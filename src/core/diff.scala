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

sealed trait Tweak[+ElemType] extends Change[ElemType]:
  def value: ElemType

object Change:
  case class Ins[+ElemType](right: Int, value: ElemType) extends Tweak[ElemType]
  case class Del[+ElemType](left: Int, value: ElemType) extends Tweak[ElemType]
  case class Keep[+ElemType](left: Int, right: Int, value: ElemType) extends Tweak[ElemType]
  
  case class Sub[+ElemType](left: Int, right: Int, leftValue: ElemType, rightValue: ElemType)
  extends Change[ElemType]

import Change.*

enum Region[ElemType]:
  case Changed(deletions: List[Del[ElemType]], insertions: List[Change.Ins[ElemType]])
  case Unchanged(retentions: List[Keep[ElemType]])

enum Chunk[+ElemType]:
  case Ins(startRight: Int, values: List[ElemType])
  case Del(startLeft: Int, values: List[ElemType])
  case Keep(startLeft: Int, startRight: Int, values: List[ElemType])
  case Sub(startLeft: Int, startRight: Int, valuesLeft: List[ElemType], valuesRight: List[ElemType])

case class RDiff[ElemType](changes: Change[ElemType]*):
  def flip: RDiff[ElemType] =
    val changes2 = changes.map:
      case Keep(left, right, value)                => Keep(right, left, value)
      case Del(left, value)                        => Ins(left, value)
      case Ins(right, value)                       => Del(right, value)
      case Sub(left, right, leftValue, rightValue) => Sub(right, left, rightValue, leftValue)
    
    RDiff[ElemType](changes2*)

case class Diff[ElemType](tweaks: Tweak[ElemType]*):
  def flip: Diff[ElemType] =
    val tweaks2 = tweaks.map:
      case Keep(left, right, value) => Keep(right, left, value)
      case Del(left, value)         => Ins(left, value)
      case Ins(right, value)        => Del(right, value)
    
    Diff[ElemType](tweaks2*)
  
  def apply(list: List[ElemType], update: (ElemType, ElemType) -> ElemType): LazyList[ElemType] =
    def recur(todo: List[Tweak[ElemType]], list: List[ElemType]): LazyList[ElemType] = todo match
      case Ins(_, value) :: tail     => value #:: recur(tail, list)
      case Del(_, _) :: tail         => recur(tail, list.tail)
      case Keep(_, _, value) :: tail => update(value, list.head) #:: recur(tail, list.tail)
      case Nil                       => LazyList()

    recur(tweaks.to(List), list)

  def rdiff2(similar: (ElemType, ElemType) -> Boolean, swapSize: Int = 1): RDiff[ElemType] =
    val changes = collate2.flatMap:
      case Region.Unchanged(keeps)   => keeps
      case Region.Changed(dels, Nil) => dels
      case Region.Changed(Nil, inss) => inss
      
      case Region.Changed(dels, inss) =>
        if inss.length == dels.length && inss.length <= swapSize
        then dels.zip(inss).map { (del, ins) => Sub(del.left, ins.right, del.value, ins.value) }
        else
          val delsSeq = dels.map(_.value).to(IndexedSeq)
          val inssSeq = inss.map(_.value).to(IndexedSeq)
          
          diff(delsSeq, inssSeq, similar).tweaks.map:
            case Del(l, v)     => Del(dels(l).left, dels(l).value)
            case Ins(r, v)     => Ins(inss(r).right, inss(r).value)
            case Keep(l, r, _) => Sub(dels(l).left, inss(r).right, dels(l).value, inss(r).value)
    
    RDiff(changes*)
          
  def rdiff(similar: (ElemType, ElemType) -> Boolean, bySize: Int = 1): RDiff[ElemType] =
    val changes = collate(similar, bySize).flatMap:
      case Chunk.Ins(rightIndex, values) =>
        values.zipWithIndex.map: (value, offset) =>
          Ins(rightIndex + offset, value)
      
      case Chunk.Del(leftIndex, values) =>
        values.zipWithIndex.map: (value, offset) =>
          Del(leftIndex + offset, value)
      
      case Chunk.Keep(leftIndex, rightIndex, values) =>
        values.zipWithIndex.map: (value, offset) =>
          Keep(leftIndex + offset, rightIndex + offset, value)
      
      case Chunk.Sub(leftIndex, rightIndex, leftValues, rightValues) =>
        leftValues.zip(rightValues).zipWithIndex.map:
          case ((leftValue, rightValue), offset) =>
            Sub(leftIndex + offset, rightIndex + offset, leftValue, rightValue)
    
    RDiff(changes*)

  def collate2: List[Region[ElemType]] =
    tweaks.runs:
      case Keep(_, _, _) => true
      case _             => false
    .map:
      case xs@(Keep(_, _, _) :: _) => val keeps = xs.collect { case keep@Keep(_, _, _) => keep }
                                      Region.Unchanged(keeps)
      case xs                      => val dels = xs.collect { case del@Del(_, _) => del }
                                      val inss = xs.collect { case ins@Ins(_, _) => ins }
                                      Region.Changed(dels, inss)
  
  def collate
      (similar: (ElemType, ElemType) -> Boolean, bySize: Int = 1)
      : List[Chunk[ElemType]] =
    tweaks.runs:
      case Keep(_, _, _) => true
      case _             => false
    .flatMap: xs =>
      (xs: @unchecked) match
        case xs@(Keep(left, right, _) :: _) => List(Chunk.Keep(left, right, xs.map(_.value)))
        case xs@(Ins(idx, _) :: _)          => List(Chunk.Ins(idx, xs.map(_.value)))
        
        case xs@(Del(leftIdx, _) :: _) =>
          val dels =
            xs.takeWhile:
              case Del(_, _) => true
              case _         => false
            .to(IndexedSeq)
          
          val delValues = dels.map(_.value).to(List)
  
          (xs.drop(dels.length): @unchecked) match
            case Nil =>
              List(Chunk.Del(leftIdx, xs.map(_.value)))
            
            case inss@(Ins(rightIdx, _) :: _) =>
              val insValues = inss.map(_.value)
    
              if dels.length <= bySize && insValues.length <= bySize
              then List(Chunk.Sub(leftIdx, rightIdx, delValues, insValues))
              else
                val delsSeq = delValues.to(IndexedSeq)
                val inssSeq = insValues.to(IndexedSeq)
                diff(delsSeq, inssSeq, similar).tweaks.runs(_.productPrefix).map: xs =>
                  (xs: @unchecked) match
                    case xs@(Ins(idx, _) :: _) => Chunk.Ins(leftIdx + idx, xs.map(_.value))
                    case xs@(Del(idx, _) :: _) => Chunk.Del(leftIdx + idx, xs.map(_.value))
                    
                    case xs@(Keep(left, right, _) :: _) =>
                      val valuesLeft = delValues.drop(left).take(xs.length)
                      val valuesRight = insValues.drop(right).take(xs.length)
                      Chunk.Sub(left + leftIdx, right + rightIdx, valuesLeft, valuesRight)


def diff
    [ElemType]
    (left: IndexedSeq[ElemType], right: IndexedSeq[ElemType],
        compare: (ElemType, ElemType) -> Boolean = { (a: ElemType, b: ElemType) => a == b })
    : Diff[ElemType] =
  @tailrec
  def count(pos: Int, off: Int): Int =
    if pos >= left.length || pos + off >= right.length || !compare(left(pos), right(pos + off)) then pos
    else count(pos + 1, off)

  @tailrec
  def trace
      (deletes: Int = 0, inserts: Int = 0, current: List[Int] = Nil, rows: List[IArray[Int]] = Nil)
      : Diff[ElemType] =
    val delPos = if deletes == 0 then 0 else count(rows.head(deletes - 1) + 1, inserts - deletes)
    val insPos = if inserts == 0 then 0 else count(rows.head(deletes), inserts - deletes)
    val best = if deletes + inserts == 0 then count(0, 0) else delPos.max(insPos)
    
    if best == left.length && (best - deletes + inserts) == right.length
    then Diff(countback(left.length - 1, deletes, rows, Nil)*)
    else if inserts > 0 then trace(deletes + 1, inserts - 1, best :: current, rows)
    else trace(0, deletes + 1, Nil, IArray.from((best :: current).reverse) :: rows)

  @tailrec
  def countback
      (pos: Int, deletes: Int, rows: List[IArray[Int]], tweaks: List[Tweak[ElemType]])
      : List[Tweak[ElemType]] =
    val rpos = pos + rows.length - deletes*2
    lazy val ins = rows.head(deletes) - 1
    lazy val del = rows.head(deletes - 1)
    
    if pos == -1 && rpos == -1 then tweaks else if rows.isEmpty
    then countback(pos - 1, deletes, rows, Keep(pos, rpos, left(pos)) :: tweaks)
    else if deletes < rows.length && (deletes == 0 || ins >= del)
    then
      if pos == ins then countback(pos, deletes, rows.tail, Ins(rpos, right(rpos)) :: tweaks)
      else countback(pos - 1, deletes, rows, Keep(pos, rpos, left(pos)) :: tweaks)
    else
      if pos == del then countback(del - 1, deletes - 1, rows.tail, Del(pos, left(pos)) :: tweaks)
      else countback(pos - 1, deletes, rows, Keep(pos, rpos, left(pos)) :: tweaks)
    
  trace()

given Realm = Realm(t"dissonance")
