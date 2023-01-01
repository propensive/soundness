/*
    Quagmire, version 0.1.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quagmire

import eucalyptus.*
import gossamer.*
import rudiments.*
import annotation.*

import language.experimental.captureChecking

enum Change[+T]:
  case Ins(right: Int, value: T)
  case Del(left: Int, value: T)
  case Keep(left: Int, right: Int, value: T)

import Change.*

case class Diff[T](changes: Change[T]*):
  def flip: Diff[T] =
    val changes2 = changes.map:
      case Keep(l, r, v) => Keep(r, l, v)
      case Del(l, v)     => Ins(l, v)
      case Ins(r, v)     => Del(r, v)
    
    Diff[T](changes2*)
  
  def apply(seq: List[T], update: (T, T) -> T): LazyList[T] =
    def recur(todo: Seq[Change[T]], seq: List[T]): LazyList[T] = todo match
      case Ins(_, value) :: tail     => value #:: recur(tail, seq)
      case Del(_, _) :: tail         => recur(tail, seq.tail)
      case Keep(_, _, value) :: tail => update(value, seq.head) #:: recur(tail, seq.tail)
      case Nil                   => LazyList()

    recur(changes, seq)

object Diff:
  import Point.*

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

  def diff[T](left: IArray[T], right: IArray[T], cmp: (T, T) -> Boolean = { (a: T, b: T) => a == b }): Diff[T] =
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

    def countback(idx: Int, cur: Point, trace: List[IArray[Point]], result: List[Change[T]] = Nil): Diff[T] =
      trace match
        case head :: tail =>
          val target = head(idx)
          
          if cur == Point(0, 0) then Diff(sort(result, Nil, Nil)*)
          else if cur.x > target.x && cur.y > target.y then
            countback(idx, cur.unkeep, trace, Keep(cur.x - 1, cur.y - 1, left(cur.x - 1)) :: result)
          else if cur == target.ins then
            countback(0 max (idx - 1), target, tail, Ins(target.y, right(target.y)) :: result)
          else if cur == target.del then
            countback(0 max (idx - 1), target, tail, Del(cur.x - 1, left(cur.x - 1)) :: result)
          else throw Mistake("Unexpected")
  
        case Nil =>
          if cur == Point(0, 0) then Diff(sort(result, Nil, Nil)*)
          else countback(0, cur.unkeep, Nil, Keep(cur.x - 1, cur.y - 1, left(cur.x - 1)) :: result)

    @tailrec
    def sort(values: List[Change[T]], cache: List[Change[T]], result: List[Change[T]]): List[Change[T]] =
      values match
        case Keep(l, r, v) :: tail => cache match
          case Nil                    => sort(tail, Nil, Keep(l, r, v) :: result)
          case cache                  => sort(tail, Nil, Keep(l, r, v) :: (cache ::: result))
        case Ins(r, v) :: tail     => sort(tail, Ins(r, v) :: cache, result)
        case Del(l, v) :: tail     => sort(tail, cache, Del(l, v) :: result)
        case Nil                   => (cache ::: result).reverse

    distance()

given Realm = Realm(t"quagmire")