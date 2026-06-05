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
package savagery

import scala.math.Numeric

import anticipation.*
import gossamer.*
import mosquito.*
import prepositional.*
import spectacular.*
import symbolism.*

object Delta:
  def apply(dx: Float, dy: Float): Delta = Delta(Vector((dx, dy)))

  given showable: Delta is Showable = delta =>
    t"${delta.dx.toString} ${delta.dy.toString}"

  given addable: Delta is Addable by Delta to Delta = Addable: (left, right) =>
    Delta(left.dx + right.dx, left.dy + right.dy)

  given subtractable: Delta is Subtractable by Delta to Delta = Subtractable: (left, right) =>
    Delta(left.dx - right.dx, left.dy - right.dy)

  given negatable: Delta is Negatable to Delta = Negatable: delta =>
    Delta(-delta.dx, -delta.dy)

  given multiplicableFloat: Float is Multiplicable by Delta to Delta = Multiplicable:
    (scalar, delta) => Delta(scalar*delta.dx, scalar*delta.dy)

  given multiplicableDouble: Double is Multiplicable by Delta to Delta = Multiplicable:
    (scalar, delta) => Delta((scalar*delta.dx).toFloat, (scalar*delta.dy).toFloat)

  given multiplicableInt: Int is Multiplicable by Delta to Delta = Multiplicable:
    (scalar, delta) => Delta(scalar*delta.dx, scalar*delta.dy)

  given multiplicableByFloat: Delta is Multiplicable by Float to Delta = Multiplicable:
    (delta, scalar) => Delta(delta.dx*scalar, delta.dy*scalar)

  given multiplicableByDouble: Delta is Multiplicable by Double to Delta = Multiplicable:
    (delta, scalar) => Delta((delta.dx*scalar).toFloat, (delta.dy*scalar).toFloat)

  given multiplicableByInt: Delta is Multiplicable by Int to Delta = Multiplicable:
    (delta, scalar) => Delta(delta.dx*scalar, delta.dy*scalar)

final case class Delta(vector: Vector[Float, 2]):
  def dx: Float = vector.element(0)
  def dy: Float = vector.element(1)

val Up:    Delta = Delta(0.0f, -1.0f)
val Down:  Delta = Delta(0.0f, 1.0f)
val Left:  Delta = Delta(-1.0f, 0.0f)
val Right: Delta = Delta(1.0f, 0.0f)

extension [numeric: Numeric, numeric2: Numeric](tuple: (numeric, numeric2))
  def `unary_+`: Delta = Delta(numeric.toFloat(tuple(0)), numeric2.toFloat(tuple(1)))

given negatableTuple: [numeric: Numeric, numeric2: Numeric]
=>  (numeric, numeric2) is Negatable to Delta =
  Negatable: tuple =>
    Delta(-numeric.toFloat(tuple(0)), -numeric2.toFloat(tuple(1)))
