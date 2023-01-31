/*
    Dissonance, version 0.1.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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
import annotation.*

import language.experimental.captureChecking

sealed trait Change[+T] extends Product

sealed trait SimpleChange[+T] extends Change[T]:
  def value: T

object Change:
  case class Ins[+T](right: Int, value: T) extends SimpleChange[T]
  case class Del[+T](left: Int, value: T) extends SimpleChange[T]
  case class Keep[+T](left: Int, right: Int, value: T) extends SimpleChange[T]
  case class Replace[+T](left: Int, right: Int, leftValue: T, rightValue: T) extends Change[T]

import Change.*

enum ChangeBlock[+T]:
  case Ins(startRight: Int, values: List[T])
  case Del(startLeft: Int, values: List[T])
  case Keep(startLeft: Int, startRight: Int, values: List[T])
  case Replace(startLeft: Int, startRight: Int, valuesLeft: List[T], valuesRight: List[T])

case class RDiff[T](changes: Change[T]*):
  def flip: RDiff[T] =
    val changes2 = changes.map:
      case Keep(l, r, v)         => Keep(r, l, v)
      case Del(l, v)             => Ins(l, v)
      case Ins(r, v)             => Del(r, v)
      case Replace(l, r, lv, rv) => Replace(r, l, rv, lv)
    
    RDiff[T](changes2*)

case class Diff[T](changes: SimpleChange[T]*):
  def flip: Diff[T] =
    val changes2 = changes.map:
      case Keep(l, r, v) => Keep(r, l, v)
      case Del(l, v)     => Ins(l, v)
      case Ins(r, v)     => Del(r, v)
    
    Diff[T](changes2*)
  
  def apply(list: List[T], update: (T, T) -> T): LazyList[T] =
    def recur(todo: List[SimpleChange[T]], list: List[T]): LazyList[T] = todo match
      case Ins(_, value) :: tail     => value #:: recur(tail, list)
      case Del(_, _) :: tail         => recur(tail, list.tail)
      case Keep(_, _, value) :: tail => update(value, list.head) #:: recur(tail, list.tail)
      case Nil                       => LazyList()

    recur(changes.to(List), list)

  def collate(similar: (T, T) -> Boolean, bySize: Int = 1): List[ChangeBlock[T]] =
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
            else diff(delValues.to(IndexedSeq), insValues.to(IndexedSeq), similar).changes.runs(_.productPrefix).map:
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
  
  def rdiff(similar: (T, T) -> Boolean, bySize: Int = 1): RDiff[T] =
    val changes = collate(similar, bySize).flatMap:
      case ChangeBlock.Ins(r, vs)              => vs.zipWithIndex.map { (v, i) => Ins(r + i, v) }
      case ChangeBlock.Del(l, vs)              => vs.zipWithIndex.map { (v, i) => Del(l + i, v) }
      case ChangeBlock.Keep(l, r, vs)          => vs.zipWithIndex.map { (v, i) => Keep(l + i, r + i, v) }
      case ChangeBlock.Replace(l, r, lvs, rvs) => lvs.zip(rvs).zipWithIndex.map:
        case ((lv, rv), i) => Replace(l + i, r + i, lv, rv)
    
    RDiff[T](changes*)

object Diff:
  object Point:
    opaque type Point = Long
    def apply(x: Int, y: Int): Point = (x.toLong << 32) + y
    
    extension (point: Point)
      def x: Int = (point >> 32).toInt
      def y: Int = point.toInt
      def del: Point = Point(x + 1, y)
      def ins: Point = Point(x, y + 1)
      def keep: Point = Point(x + 1, y + 1)
      def unkeep: Point = Point(x - 1, y - 1)
      def text: Text = t"[$x,$y]"

import Diff.Point, Point.*

def diff[T](left: IndexedSeq[T], right: IndexedSeq[T], cmp: (T, T) -> Boolean = { (a: T, b: T) => a == b }): Diff[T] =
  val end = Point(left.size, right.size)
  @tailrec
  def distance(last: IArray[Point] = IArray(count(Point(0, 0))), trace: List[IArray[Point]] = Nil): Diff[T] =
    if last.contains(end) then
      val idx = last.indexOf(end)

      if trace.isEmpty then countback(idx, end, Nil)
      else if trace.head.length > idx && count(trace.head(idx).ins) == end then countback(idx, end, trace)
      else countback(idx - 1, end, trace)

    else
      val round = last.size
      val next = IArray.create[Point](round + 1): arr =>
        arr(0) = last(0).ins

        last.indices.foreach: i =>
          arr(i + 1) = count(last(i).del)
          count(last(i).ins).pipe { pt => if i == round || pt.x > arr(i).x then arr(i) = pt }

      distance(next, last :: trace)

  @tailrec
  def count(pt: Point): Point =
    if pt.x >= left.size || pt.y >= right.size || !cmp(left(pt.x), right(pt.y)) then pt else count(pt.keep)

  def countback(idx: Int, cur: Point, trace: List[IArray[Point]], result: List[SimpleChange[T]] = Nil): Diff[T] =
    trace match
      case head :: tail =>
        val target = head(idx)
        
        if cur == Point(0, 0) then Diff(result*)
        else if cur.x > target.x && cur.y > target.y then
          countback(idx, cur.unkeep, trace, Keep(cur.x - 1, cur.y - 1, left(cur.x - 1)) :: result)
        else if cur == target.ins then
          countback(idx min (tail.length - 1), target, tail, Ins(target.y, right(target.y)) :: result)
        else if cur == target.del then
          countback(0 max (idx - 1), target, tail, Del(cur.x - 1, left(cur.x - 1)) :: result)
        else throw Mistake(s"Unexpected: idx=$idx cur=${cur.text} result=${result}")

      case Nil =>
        if cur == Point(0, 0) then Diff(result*)
        else countback(0, cur.unkeep, Nil, Keep(cur.x - 1, cur.y - 1, left(cur.x - 1)) :: result)

  distance()

given Realm = Realm(t"dissonance")
