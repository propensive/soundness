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
import spectacular.*

import language.experimental.captureChecking

sealed trait Change[+ElemType] extends Product

sealed trait SimpleChange[+ElemType] extends Change[ElemType]:
  def value: ElemType

object Change:
  case class Ins[+ElemType](right: Int, value: ElemType) extends SimpleChange[ElemType]
  case class Del[+ElemType](left: Int, value: ElemType) extends SimpleChange[ElemType]
  case class Keep[+ElemType](left: Int, right: Int, value: ElemType) extends SimpleChange[ElemType]
  case class Replace[+ElemType](left: Int, right: Int, leftValue: ElemType, rightValue: ElemType) extends Change[ElemType]

import Change.*

enum Region[ElemType]:
  case Changed(deletions: List[Change.Del[ElemType]], insertions: List[Change.Ins[ElemType]])
  case Unchanged(retentions: List[Change.Keep[ElemType]])

enum ChangeBlock[+ElemType]:
  case Ins(startRight: Int, values: List[ElemType])
  case Del(startLeft: Int, values: List[ElemType])
  case Keep(startLeft: Int, startRight: Int, values: List[ElemType])
  case Replace(startLeft: Int, startRight: Int, valuesLeft: List[ElemType], valuesRight: List[ElemType])

case class RDiff[ElemType](changes: Change[ElemType]*):
  def flip: RDiff[ElemType] =
    val changes2 = changes.map:
      case Keep(l, r, v)         => Keep(r, l, v)
      case Del(l, v)             => Ins(l, v)
      case Ins(r, v)             => Del(r, v)
      case Replace(l, r, lv, rv) => Replace(r, l, rv, lv)
    
    RDiff[ElemType](changes2*)

case class Diff[ElemType](changes: SimpleChange[ElemType]*):
  def flip: Diff[ElemType] =
    val changes2 = changes.map:
      case Keep(l, r, v) => Keep(r, l, v)
      case Del(l, v)     => Ins(l, v)
      case Ins(r, v)     => Del(r, v)
    
    Diff[ElemType](changes2*)
  
  def apply(list: List[ElemType], update: (ElemType, ElemType) -> ElemType): LazyList[ElemType] =
    def recur(todo: List[SimpleChange[ElemType]], list: List[ElemType]): LazyList[ElemType] = todo match
      case Ins(_, value) :: tail     => value #:: recur(tail, list)
      case Del(_, _) :: tail         => recur(tail, list.tail)
      case Keep(_, _, value) :: tail => update(value, list.head) #:: recur(tail, list.tail)
      case Nil                       => LazyList()

    recur(changes.to(List), list)

  def collate2: List[Region[ElemType]] =
    changes.runs:
      case Keep(_, _, _) => true
      case _             => false
    .map:
      case xs@(Keep(_, _, _) :: _) => val keeps = xs.collect { case keep@Keep(_, _, _) => keep }
                                      Region.Unchanged(keeps)
      case xs                      => val dels = xs.collect { case del@Del(_, _) => del }
                                      val inss = xs.collect { case ins@Ins(_, _) => ins }
                                      Region.Changed(dels, inss)
  
  def rdiff2(similar: (ElemType, ElemType) -> Boolean, swapSize: Int = 1): RDiff[ElemType] =
    val changes = collate2.flatMap:
      case Region.Unchanged(keeps)   => keeps
      case Region.Changed(dels, Nil) => dels
      case Region.Changed(Nil, inss) => inss
      
      case Region.Changed(dels, inss) =>
        if inss.length == dels.length && inss.length <= swapSize
        then dels.zip(inss).map: (del, ins) =>
          Change.Replace[ElemType](del.left, ins.right, del.value, ins.value)
        else
          diff(dels.map(_.value).to(IndexedSeq), inss.map(_.value).to(IndexedSeq), similar).changes.map:
            case Keep(l, r, _) =>
              Change.Replace[ElemType](dels(l).left, inss(r).right, dels(l).value, inss(r).value)
            
            case Del(l, v) =>
              Change.Del[ElemType](dels(l).left, dels(l).value)
            
            case Ins(r, v) =>
              Change.Ins[ElemType](inss(r).right, inss(r).value)
    
    RDiff(changes*)
          
  def rdiff(similar: (ElemType, ElemType) -> Boolean, bySize: Int = 1): RDiff[ElemType] =
    val changes = collate(similar, bySize).flatMap:
      case ChangeBlock.Ins(rightIndex, values) =>
        values.zipWithIndex.map: (value, offset) =>
          Ins(rightIndex + offset, value)
      
      case ChangeBlock.Del(leftIndex, values) =>
        values.zipWithIndex.map: (value, offset) =>
          Del(leftIndex + offset, value)
      
      case ChangeBlock.Keep(leftIndex, rightIndex, values) =>
        values.zipWithIndex.map: (value, offset) =>
          Keep(leftIndex + offset, rightIndex + offset, value)
      
      case ChangeBlock.Replace(leftIndex, rightIndex, leftValues, rightValues) =>
        leftValues.zip(rightValues).zipWithIndex.map:
          case ((leftValue, rightValue), offset) =>
            Replace(leftIndex + offset, rightIndex + offset, leftValue, rightValue)
    
    RDiff(changes*)

  def collate
      (similar: (ElemType, ElemType) -> Boolean, bySize: Int = 1)
      : List[ChangeBlock[ElemType]] =
    changes.runs:
      case Keep(_, _, _) => true
      case _             => false
    .flatMap:
      case xs@(Keep(left, right, _) :: _) => List(ChangeBlock.Keep(left, right, xs.map(_.value)))
      case xs@(Ins(idx, _) :: _)          => List(ChangeBlock.Ins(idx, xs.map(_.value)))
      
      case xs@(Del(leftIdx, _) :: _) =>
        val dels =
          xs.takeWhile:
            case Del(_, _) => true
            case _         => false
          .to(IndexedSeq)
        
        val delValues = dels.map(_.value).to(List)

        xs.drop(dels.length) match
          case Nil =>
            List(ChangeBlock.Del(leftIdx, xs.map(_.value)))
          
          case inss@(Ins(rightIdx, _) :: _) =>
            val insValues = inss.map(_.value)
  
            if dels.length <= bySize && insValues.length <= bySize
            then List(ChangeBlock.Replace(leftIdx, rightIdx, delValues, insValues))
            else diff(delValues.to(IndexedSeq), insValues.to(IndexedSeq),
                similar).changes.runs(_.productPrefix).map:
              case xs@(Ins(idx, _) :: _) => ChangeBlock.Ins(leftIdx + idx, xs.map(_.value))
              case xs@(Del(idx, _) :: _) => ChangeBlock.Del(leftIdx + idx, xs.map(_.value))
              
              case xs@(Keep(left, right, _) :: _) =>
                val valuesLeft = delValues.drop(left).take(xs.length)
                val valuesRight = insValues.drop(right).take(xs.length)
                ChangeBlock.Replace(left + leftIdx, right + rightIdx, valuesLeft, valuesRight)
              case Nil =>
                throw Mistake("Should never have an empty list here")
          case _ =>
            throw Mistake("Should never have an empty list here")

      case Nil =>
        throw Mistake("Should never have an empty list here")


def diff
    [ElemType]
    (left: IndexedSeq[ElemType], right: IndexedSeq[ElemType],
        compare: (ElemType, ElemType) -> Boolean = { (a: ElemType, b: ElemType) => a == b })
    : Diff[ElemType] =
  println(s"\ndiff(${left.debug}, ${right.debug})")

  val leftMax = left.length
  val rightMax = right.length

  @tailrec
  def count(leftIndex: Int, rightIndex: Int, total: Int = 0): Int =
    //println(s"count($leftIndex, $rightIndex, $total)")
    if leftIndex < leftMax && rightIndex < rightMax && compare(left(leftIndex), right(rightIndex))
    then count(leftIndex + 1, rightIndex + 1, total + 1)
    else total

  @tailrec
  def trace
      (dels: Int = 0, inss: Int = 0, rows: List[Array[Int]] = List(Array(0)))
      : Diff[ElemType] =
    (rows: @unchecked) match
      case head :: tail =>
        val deletion = if dels == 0 then 0 else
          val leftIndex = math.abs(tail.head(dels - 1)) + 1
          val rightIndex = leftIndex - dels + inss

          leftIndex + count(leftIndex, rightIndex)
        
        val insertion = if inss == 0 then 0 else
          val leftIndex = math.abs(tail.head(dels))
          val rightIndex = leftIndex - dels + inss
          
          leftIndex + count(leftIndex, rightIndex)
        
        val best = if dels + inss == 0 then count(0, 0) else if deletion > insertion then deletion else insertion
        
        if best == leftMax && (best - dels + inss) == rightMax
        then Diff(countback(leftMax, if deletion > insertion then -best else best, dels, tail, Nil)*)
        else head(dels) = if deletion > insertion then -best else best
  
        if inss == 0 then trace(0, dels + 1, new Array[Int](dels + 2) :: rows)
        else trace(dels + 1, inss - 1, rows)

  @tailrec
  def countback
      (pos: Int, target: Int, dels: Int, rows: List[Array[Int]], changes: List[SimpleChange[ElemType]])
      : List[SimpleChange[ElemType]] =
    println("")
    println(changes.debug)
    println(rows.debug)
    val k = rows.length
    val inss = k - dels
    
    val leftIndex = pos
    val rightIndex = leftIndex - dels + inss
    println(s"($leftIndex,$rightIndex): pos=$pos, dels=$dels, inss=$inss, target=$target")
    
    if pos == 0 then changes
    else if pos == math.abs(target) then
      val next = if target > 0 then rows.head(dels) else rows.head(dels - 1)
      if target > 0 then countback(pos, next, dels, rows.tail, Ins(rightIndex - 1, right(rightIndex - 1)) :: changes)
      else countback(pos - 1, next, dels - 1, rows.tail, Del(leftIndex - 1, left(leftIndex - 1)) :: changes)
    else if rightIndex == math.abs(target) then
      val next = if target > 0 then rows.head(dels) else rows.head(dels - 1)
      if target > 0 then countback(pos, next, dels, rows.tail, Ins(rightIndex - 1, right(rightIndex - 1)) :: changes)
      else countback(pos - 1, next, dels - 1, rows.tail, Del(leftIndex - 1, left(leftIndex - 1)) :: changes)
    else if pos > math.abs(target)
    then countback(pos - 1, target, dels, rows, Keep(leftIndex - 1, rightIndex - 1, left(leftIndex - 1)) :: changes)
    else ??? // countback(pos - 1, dels - 1, rows.tail, Del(leftIndex, left(leftIndex)) :: changes)

  trace()

given Realm = Realm(t"dissonance")
