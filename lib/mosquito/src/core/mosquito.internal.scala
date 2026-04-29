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
package mosquito

import scala.annotation.targetName
import scala.compiletime.*

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object internal:
  object Tensor:
    def apply(tuple: Tuple): Tensor[Tuple.Union[tuple.type], Tuple.Size[tuple.type]] =
      new Tensor(tuple.toIArray)

    def take[element](list: List[element], size: Int): Optional[Tensor[element, size.type]] =
      val array: Array[Any] = new Array(size)
      var i = 0
      var rest = list
      while i < size do
        rest match
          case Nil =>
            return Unset

          case head :: tail =>
            array(i) = head
            rest = tail
            i += 1

      new Tensor[element, size.type](array.immutable(using Unsafe))


    given addable
    :   [ value,
          size   <: Int,
          left   <: Tensor[value, size],
          value2,
          right  <: Tensor[value2, size],
          result ]
    =>  ( addable: value is Addable by value2 to result )
    =>  left is Addable:

      type Operand = right
      type Result = Tensor[result, size]

      def add(left: left, right: right): Tensor[result, size] =
        val length = left.data.length
        val arr = IArray.build[Any](length): array =>
          var i = 0
          while i < length do
            array(i) =
              addable.add(left.data(i).asInstanceOf[value], right.data(i).asInstanceOf[value2])

            i += 1

        new Tensor[result, size](arr)


    given negatable: [value, size <: Int, tensor <: Tensor[value, size], result]
    =>  ( negatable: value is Negatable to result )
    =>  tensor is Negatable:

      type Result = Tensor[result, size]

      def negate(operand: tensor): Tensor[result, size] = operand.map(negatable.negate(_))


    given subtractable
    :   [ value,
          size   <: Int,
          left   <: Tensor[value, size],
          value2,
          right  <: Tensor[value2, size],
          result ]
    =>  ( subtractable: value is Subtractable by value2 to result )
    =>  left is Subtractable:

        type Self = left
        type Operand = right
        type Result = Tensor[result, size]

        def subtract(left: left, right: right): Tensor[result, size] =
          val length = left.data.length
          val arr = IArray.build[Any](length): array =>
            var i = 0
            while i < length do
              array(i) =
                subtractable.subtract
                  ( left.data(i).asInstanceOf[value], right.data(i).asInstanceOf[value2] )

              i += 1

          new Tensor[result, size](arr)


    given showable: [size <: Int: ValueOf, element: Showable] => Text is Measurable
    =>  Tensor[element, size] is Showable =

      tensor =>
        val items = tensor.list.map(_.show)
        val width = items.maxBy(_.length).length
        val size = valueOf[size]
        if size == 1 then t"( ${items(0)} )"
        else
          val top = t"⎛ ${items.head.pad(width, Rtl)} ⎞"
          val bottom = t"⎝ ${items.last.pad(width, Rtl)} ⎠"
          val middle = items.tail.init.map: item => t"⎜ ${item.pad(width, Rtl)} ⎟"

          (top :: middle ::: bottom :: Nil).join(t"\n")


  extension [left](left: Tensor[left, 3])
    def cross[right](right: Tensor[right, 3])
      ( using multiplication: left is Multiplicable by right,
              addition:       multiplication.Result is Addable by multiplication.Result,
              subtraction:    multiplication.Result is Subtractable by multiplication.Result )
    :   Tensor[addition.Result, 3] =

      val first = left.element(1)*right.element(2) - left.element(2)*right.element(1)
      val second = left.element(2)*right.element(0) - left.element(0)*right.element(2)
      val third = left.element(0)*right.element(1) - left.element(1)*right.element(0)

      new Tensor[addition.Result, 3](IArray[Any](first, second, third))


  extension [left](left: Tensor[left, 7])
    @targetName("cross7")
    def cross[right](right: Tensor[right, 7])
      ( using multiplication: left is Multiplicable by right,
              addition:       multiplication.Result is Addable by multiplication.Result,
              subtraction:    multiplication.Result is Subtractable by multiplication.Result,
              addEq:          addition.Result =:= multiplication.Result,
              subEq:          subtraction.Result =:= multiplication.Result )
    :   Tensor[addition.Result, 7] =

      val a0 = left.element(0); val a1 = left.element(1); val a2 = left.element(2)
      val a3 = left.element(3); val a4 = left.element(4); val a5 = left.element(5)
      val a6 = left.element(6)

      val b0 = right.element(0); val b1 = right.element(1); val b2 = right.element(2)
      val b3 = right.element(3); val b4 = right.element(4); val b5 = right.element(5)
      val b6 = right.element(6)

      def combine
        ( p1: multiplication.Result, n1: multiplication.Result,
          p2: multiplication.Result, n2: multiplication.Result,
          p3: multiplication.Result, n3: multiplication.Result )
      :   multiplication.Result =

        var acc: multiplication.Result = p1
        acc = acc - n1
        acc = acc + p2
        acc = acc - n2
        acc = acc + p3
        acc = acc - n3
        acc

      val c0 = combine(a1*b3, a3*b1, a2*b6, a6*b2, a4*b5, a5*b4)
      val c1 = combine(a2*b4, a4*b2, a3*b0, a0*b3, a5*b6, a6*b5)
      val c2 = combine(a3*b5, a5*b3, a4*b1, a1*b4, a6*b0, a0*b6)
      val c3 = combine(a4*b6, a6*b4, a5*b2, a2*b5, a0*b1, a1*b0)
      val c4 = combine(a5*b0, a0*b5, a6*b3, a3*b6, a1*b2, a2*b1)
      val c5 = combine(a6*b1, a1*b6, a0*b4, a4*b0, a2*b3, a3*b2)
      val c6 = combine(a0*b2, a2*b0, a1*b5, a5*b1, a3*b4, a4*b3)

      new Tensor[addition.Result, 7](IArray[Any](c0, c1, c2, c3, c4, c5, c6))


  extension [size <: Int, left](left: Tensor[left, size])
    def element(index: Int): left = left.data(index).asInstanceOf[left]

    def apply(index: Int): left = left.data(index).asInstanceOf[left]
    def list: List[left] = left.data.toList.asInstanceOf[List[left]]
    def iarray: IArray[left] = left.data.asInstanceOf[IArray[left]]
    def size(using ValueOf[size]): Int = valueOf[size]


    def norm
      ( using multiplicable:  left is Multiplicable by left,
              addable:        multiplicable.Result is Addable by multiplicable.Result
                              to multiplicable.Result,
              rootable:       multiplicable.Result is Rootable[2] to left )
    :   left =

      def recur(sum: multiplicable.Result, i: Int): left =
        if i == 0 then sum.sqrt else
          val x2: multiplicable.Result = left.element(i)*left.element(i)
          recur(addable.add(sum, x2), i - 1)

      recur(left.element(0)*left.element(0), left.data.length - 1)


    def map[left2](fn: left => left2): Tensor[left2, size] =
      val length = left.data.length
      val arr = IArray.build[Any](length): array =>
        var i = 0
        while i < length do
          array(i) = fn(left.data(i).asInstanceOf[left])
          i += 1

      new Tensor[left2, size](arr)


    def unitary[square]
      ( using multiplicable: left is Multiplicable by left to square,
              addable:       square is Addable by square to square,
              rootable:      square is Rootable[2] to left,
              divisible:     left is Divisible by left to Double )
    :   Tensor[Double, size] =

      val magnitude: left = left.norm
      val length = left.data.length
      val arr = IArray.build[Any](length): array =>
        var i = 0
        while i < length do
          array(i) = left.data(i).asInstanceOf[left]/magnitude
          i += 1

      new Tensor[Double, size](arr)


    def dot[right](right: Tensor[right, size])
      ( using multiply: left is Multiplicable by right,
              size:     ValueOf[size],
              addable:  multiply.Result is Addable by multiply.Result,
              equality: addable.Result =:= multiply.Result )
    :   multiply.Result =

      def recur(index: Int, sum: multiply.Result): multiply.Result =
        if index < 0 then sum
        else recur(index - 1, addable.add(sum, left.element(index)*right.element(index)))

      val start = size.value - 1
      recur(start - 1, left.element(start)*right.element(start))

  class Tensor[value, size <: Int](val data: IArray[Any]):
    override def equals(right: Any): Boolean = right.asMatchable match
      case that: Tensor[?, ?] => data.sameElements(that.data)
      case _                  => false

    override def hashCode: Int =
      scala.util.hashing.MurmurHash3.arrayHash(data.mutable(using Unsafe))

    override def toString: String = data.mkString("Tensor(", ", ", ")")
