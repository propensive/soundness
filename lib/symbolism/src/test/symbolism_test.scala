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
package symbolism

import soundness.*

case class Vector2(x: Int, y: Int)

object Vector2:
  given addable: Vector2 is Addable by Vector2 to Vector2 =
    Addable((left, right) => Vector2(left.x + right.x, left.y + right.y))

  given subtractable: Vector2 is Subtractable by Vector2 to Vector2 =
    Subtractable((left, right) => Vector2(left.x - right.x, left.y - right.y))

  given multiplicable: Vector2 is Multiplicable by Int to Vector2 =
    Multiplicable((vector, scale) => Vector2(vector.x*scale, vector.y*scale))

  given divisible: Vector2 is Divisible by Int to Vector2 =
    Divisible((vector, divisor) => Vector2(vector.x/divisor, vector.y/divisor))

  given negatable: Vector2 is Negatable to Vector2 =
    Negatable(vector => Vector2(-vector.x, -vector.y))

  given zeroic: Vector2 is Zeroic:
    def zero: Vector2 = Vector2(0, 0)

  given unital: Vector2 is Unital:
    protected def makeOne(): Vector2 = Vector2(1, 1)

case class Chunk(text: Text)

object Chunk:
  given concatenable: Chunk is Concatenable by Chunk to Chunk =
    (left, right) => Chunk(left.text+right.text)

  given zeroic: Chunk is Zeroic:
    def zero: Chunk = Chunk(t"")

case class Fraction(numerator: Int, denominator: Int)

object Fraction:
  given quotient: Fraction is Quotient:
    type Topic = Int
    type Transport = Int

    def decompose(fraction: Fraction): Option[(Int, Int)] =
      if fraction.denominator == 0 then None
      else Some((fraction.numerator, fraction.denominator))

object Tests extends Suite(m"Symbolism Tests"):
  def run(): Unit =
    suite(m"Addable tests"):
      test(m"Add two Ints through the typeclass"):
        Addable.int.add(2, 3)
      . assert(_ == 5)

      test(m"Add two Doubles through the typeclass"):
        Addable.double.add(0.25, 0.5)
      . assert(_ == 0.75)

      test(m"Adding an Int to a Double widens the result"):
        Addable.int2.add(3, 0.5)
      . assert(_ == 3.5)

      test(m"Adding a Double to an Int widens the result"):
        Addable.int3.add(0.5, 3)
      . assert(_ == 3.5)

      test(m"Adding two Shorts narrows back to a Short"):
        Addable.short.add(1, 2)
      . assert(_ == 3.toShort)

      test(m"Adding two Shorts wraps on overflow"):
        Addable.short.add(32767.toShort, 1.toShort)
      . assert(_ == (-32768).toShort)

      test(m"Adding two Bytes wraps on overflow"):
        Addable.byte.add(127.toByte, 1.toByte)
      . assert(_ == (-128).toByte)

      test(m"Add two user-defined values with the `+` operator"):
        Vector2(1, 2) + Vector2(10, 20)
      . assert(_ == Vector2(11, 22))

      test(m"A Concatenable value is Addable"):
        Chunk(t"one") + Chunk(t"two")
      . assert(_ == Chunk(t"onetwo"))

      test(m"Adding mismatched types is rejected"):
        demilitarize:
          Vector2(1, 2) + 1
      . assert(_.nonEmpty)

    suite(m"Subtractable tests"):
      test(m"Subtract two Ints through the typeclass"):
        Subtractable.int.subtract(7, 3)
      . assert(_ == 4)

      test(m"Subtract two Longs through the typeclass"):
        Subtractable.long.subtract(7L, 9L)
      . assert(_ == -2L)

      test(m"Subtracting two Bytes narrows back to a Byte"):
        Subtractable.byte.subtract(3.toByte, 5.toByte)
      . assert(_ == (-2).toByte)

      test(m"Subtract two user-defined values with the `-` operator"):
        Vector2(10, 20) - Vector2(1, 2)
      . assert(_ == Vector2(9, 18))

    suite(m"Multiplicable tests"):
      test(m"Multiply two Ints through the typeclass"):
        Multiplicable.int.multiply(6, 7)
      . assert(_ == 42)

      test(m"Multiply two Doubles through the typeclass"):
        Multiplicable.double.multiply(1.5, 4.0)
      . assert(_ == 6.0)

      test(m"Multiplying two Shorts stays within Short range"):
        Multiplicable.short.multiply(100.toShort, 3.toShort)
      . assert(_ == 300.toShort)

      test(m"Multiplying two Bytes narrows back to a Byte"):
        Multiplicable.byte.multiply(10.toByte, 4.toByte)
      . assert(_ == 40.toByte)

      test(m"Scale a user-defined value with the `*` operator"):
        Vector2(3, 4)*3
      . assert(_ == Vector2(9, 12))

      test(m"Repeat a Concatenable value with the `*` operator"):
        Chunk(t"ab")*3
      . assert(_ == Chunk(t"ababab"))

      test(m"Repeating a Concatenable value zero times gives zero"):
        Chunk(t"ab")*0
      . assert(_ == Chunk(t""))

    suite(m"Divisible tests"):
      test(m"Divide two Ints through the typeclass"):
        Divisible.int.divide(7, 2)
      . assert(_ == 3)

      test(m"Dividing an Int by a Double widens the result"):
        Divisible.int2.divide(7, 2.0)
      . assert(_ == 3.5)

      test(m"Divide a user-defined value with the `/` operator"):
        Vector2(9, 12)/3
      . assert(_ == Vector2(3, 4))

    suite(m"Negatable tests"):
      test(m"Negate an Int through the typeclass"):
        Negatable.int.negate(5)
      . assert(_ == -5)

      test(m"Negating a Byte narrows back to a Byte"):
        Negatable.byte.negate((-128).toByte)
      . assert(_ == (-128).toByte)

      test(m"Negate a user-defined value with unary minus"):
        -Vector2(3, -4)
      . assert(_ == Vector2(-3, 4))

    suite(m"Rootable tests"):
      test(m"Take the square root of a Double"):
        16.0.sqrt
      . assert(_ == 4.0)

      test(m"Take the cube root of a Double"):
        27.0.cbrt
      . assert(_ == 3.0)

      test(m"Take the square root of a Float"):
        16.0f.sqrt
      . assert(_ == 4.0f)

      test(m"Take the cube root of a Float"):
        (-27.0f).cbrt
      . assert(_ == -3.0f)

      test(m"A user-defined root can be summoned"):
        given fourth: Double is Rootable[4] to Double = Rootable(_.sqrt.sqrt)
        summon[Double is Rootable[4] to Double].root(81.0)
      . assert(_ == 3.0)

      test(m"There is no square root for Ints"):
        demilitarize:
          16.sqrt
      . assert(_.nonEmpty)

    suite(m"Zeroic and Unital tests"):
      test(m"Zero for Int is 0"):
        zero[Int]
      . assert(_ == 0)

      test(m"Zero for Double is 0.0"):
        zero[Double]
      . assert(_ == 0.0)

      test(m"Zero for String is empty"):
        zero[String]
      . assert(_ == "")

      test(m"Zero for a user-defined type"):
        zero[Vector2]
      . assert(_ == Vector2(0, 0))

      test(m"One for Int is 1"):
        summon[Int is Unital].one
      . assert(_ == 1)

      test(m"One for Float is 1.0"):
        summon[Float is Unital].one
      . assert(_ == 1.0f)

      test(m"One for a user-defined type"):
        summon[Vector2 is Unital].one
      . assert(_ == Vector2(1, 1))

      test(m"There is no zero for an arbitrary type"):
        demilitarize:
          zero[Vector2 => Vector2]
      . assert(_.nonEmpty)

    suite(m"Quotient tests"):
      test(m"Decompose a quotient into its parts"):
        Fraction(3, 4) match
          case numerator /: denominator => (numerator, denominator)
          case _                        => (0, 0)
      . assert(_ == (3, 4))

      test(m"An undecomposable quotient does not match"):
        Fraction(3, 0) match
          case numerator /: denominator => t"matched"
          case _                        => t"unmatched"
      . assert(_ == t"unmatched")
