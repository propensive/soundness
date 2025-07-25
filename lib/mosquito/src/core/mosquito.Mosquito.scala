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
┃    Soundness, version 0.40.0.                                                                    ┃
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
package mosquito

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import spectacular.*
import symbolism.*
import vacuous.*

object Mosquito:
  opaque type Vector[value, size <: Int] = Tuple

  object Vector:
    def apply(elems: Tuple): Vector[Tuple.Union[elems.type], Tuple.Size[elems.type]] = elems

    def take[element](list: List[element], size: Int): Optional[Vector[element, size.type]] =
      if size == 0 then Zero else list match
        case Nil          => Unset
        case head :: tail => take(tail, size - 1).let(head *: _)


    given addable: [value,
                    size <: Int,
                    left <: Vector[value, size],
                    value2,
                    right <: Vector[value2, size]]
          => (addable: value is Addable by value2)
          => left is Addable:
      type Self = left
      type Operand = right
      type Result = Vector[addable.Result, size]

      def add(left: left, right: right): Vector[addable.Result, size] =
        def recur(left: Tuple, right: Tuple): Tuple = left match
          case leftHead *: leftTail => right match
            case rightHead *: rightTail =>
              (leftHead.asInstanceOf[value] + rightHead.asInstanceOf[value2])
              *: recur(leftTail, rightTail)

            case _ =>
              Zero

          case _ =>
            Zero

        recur(left, right)

    given subtractable: [value,
                         size <: Int,
                         left <: Vector[value, size],
                         value2,
                         right <: Vector[value2, size]]
          => (subtractable: value is Subtractable by value2)
          => left is Subtractable:
      type Self = left
      type Operand = right
      type Result = Vector[subtractable.Result, size]

      def subtract(left: left, right: right): Vector[subtractable.Result, size] =
        def recur(left: Tuple, right: Tuple): Tuple = left match
          case leftHead *: leftTail => right match
            case rightHead *: rightTail =>
              (leftHead.asInstanceOf[value] - rightHead.asInstanceOf[value2])
              *: recur(leftTail, rightTail)

            case _ =>
              Zero

          case _ =>
            Zero

        recur(left, right)

    given showable: [size <: Int: ValueOf, element: Showable] => Text is Measurable
          =>  Vector[element, size] is Showable =

      vector =>
        val items = vector.list.map(_.show)
        val width = items.maxBy(_.length).length
        val size = valueOf[size]
        if size == 1 then t"( ${items(0)} )"
        else
          val top = t"⎛ ${items.head.pad(width, Rtl)} ⎞"
          val bottom = t"⎝ ${items.last.pad(width, Rtl)} ⎠"

          val middle = items.tail.init.map: item =>
            t"⎜ ${item.pad(width, Rtl)} ⎟"

          (top :: middle ::: bottom :: Nil).join(t"\n")

  extension [left](left: Vector[left, 3])
    def cross[right](right: Vector[right, 3])
         (using multiplication: left is Multiplicable by right,
                addition:       multiplication.Result is Addable by multiplication.Result,
                subtraction:    multiplication.Result is Subtractable by multiplication.Result)
    : Vector[addition.Result, 3] =

        val first = left.element(1)*right.element(2) - left.element(2)*right.element(1)
        val second = left.element(2)*right.element(0) - left.element(0)*right.element(2)
        val third = left.element(0)*right.element(1) - left.element(1)*right.element(0)

        first *: second *: third *: Zero


  extension [size <: Int, left](left: Vector[left, size])
    def element(index: Int): left = left.toArray(index).asInstanceOf[left]
    def apply(index: Int): left = left.toArray(index).asInstanceOf[left]
    def list: List[left] = left.toList.asInstanceOf[List[left]]
    def iarray: IArray[left] = left.toIArray.asInstanceOf[IArray[left]]
    def size(using ValueOf[size]): Int = valueOf[size]


    def norm
         (using multiplicable: left is Multiplicable by left,
                addable:       multiplicable.Result is Addable by multiplicable.Result
                                to multiplicable.Result,
                rootable:      multiplicable.Result is Rootable[2] to left)
    : left =

        def recur(sum: multiplicable.Result, i: Int): left =
          if i == 0 then sum.sqrt else
            val x2: multiplicable.Result = left.element(i)*left.element(i)
            recur(addable.add(sum, x2), i - 1)

        recur(left.element(0)*left.element(0), size - 1)


    def map[left2](fn: left => left2): Vector[left2, size] =
      def recur(tuple: Tuple): Tuple = tuple match
        case head *: tail => fn(head.asInstanceOf[left]) *: recur(tail)
        case _            => Zero

      recur(left)


    def unitary[square]
         (using multiplicable: left is Multiplicable by left to square,
                addable:       square is Addable by square to square,
                rootable:      square is Rootable[2] to left,
                divisible:     left is Divisible by left to Double)
    : Vector[Double, size] =

        val magnitude: left = left.norm

        def recur(tuple: Tuple): Tuple = tuple match
          case head *: tail => (head.asInstanceOf[left]/magnitude) *: recur(tail)
          case _            => Zero

        recur(left)


    def dot[right](right: Vector[right, size])
         (using multiply: left is Multiplicable by right,
                size:     ValueOf[size],
                addable:  multiply.Result is Addable by multiply.Result,
                equality: addable.Result =:= multiply.Result)
    : multiply.Result =

        def recur(index: Int, sum: multiply.Result): multiply.Result =
          if index < 0 then sum
          else recur(index - 1, addable.add(sum, left.element(index)*right.element(index)))

        val start = size.value - 1
        recur(start - 1, left.element(start)*right.element(start))


extension [element](list: List[element])
  def slide(size: Int): Stream[Vector[element, size.type]] = list match
    case Nil          => Stream()
    case head :: tail => Vector.take(list, size).lay(Stream())(_ #:: tail.slide(size))
