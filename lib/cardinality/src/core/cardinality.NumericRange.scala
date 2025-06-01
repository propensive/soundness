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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package cardinality

import scala.compiletime.ops.double.*
import scala.reflect.TypeTest
import scala.util.FromDigits

import language.experimental.genericNumberLiterals

import Cardinality.{Asym, Min4, Max4}

object NumericRange:
  @annotation.targetName("Range")
  opaque infix type ~ [min <: Double, max <: Double] = Double

  def apply[min <: Double, max <: Double](value: Double): min ~ max =
    value

  @annotation.targetName("Range")
  object `~`:
    given comparable: [min <: Double & Singleton, max <: Double & Singleton]
          => (min: ValueOf[min], max: ValueOf[max])
          =>  TypeTest[Double, min ~ max] =

      value =>
        if value >= min.value && value <= max.value
        then Some(value.asInstanceOf[(min ~ max) & value.type])
        else None

    class RangeParser[min <: Double, max <: Double]
    extends FromDigits.Decimal[min ~ max]:
      def fromDigits(digits: String): Double = apply(digits.toDouble)

    given cardinality: [min <: Double, max <: Double] => RangeParser[min, max]:
      override inline def fromDigits(digits: String): min ~ max =
        ${Cardinality('digits)}

    extension [leftMin <: Double, leftMax <: Double](left: leftMin ~ leftMax)
      def double: Double = left


      @annotation.targetName("add")
      infix def + [rightMin <: Double, rightMax <: Double](right: rightMin ~ rightMax)
      : (leftMin + rightMin) ~ (leftMax + rightMax) =

          left + right


      @annotation.targetName("add2")
      infix def + [E <: Double & Singleton](right: E)
      : (leftMin + right.type) ~ (leftMax + right.type) =

          left + right


      @annotation.targetName("add3")
      infix def + (right: Double): Double = left + right

      @annotation.targetName("times")
      infix def * [rightMin <: Double, rightMax <: Double](right: rightMin ~ rightMax)
      : (Min4[leftMin*rightMin, leftMin*rightMax, leftMax*rightMax, leftMax*rightMin])
        ~ (Max4[leftMin*rightMin, leftMin*rightMax, leftMax*rightMax, leftMax*rightMin]) =

        left*right


      @annotation.targetName("times2")
      infix def * [right <: Double & Singleton](right: right)
      : Min[leftMin*right, leftMax*right] ~ Max[leftMin*right, leftMax*right] =

          left*right


      @annotation.targetName("times3")
      infix def * (right: Double): Double = left*right


      @annotation.targetName("minus")
      infix def - [rightMin <: Double, rightMax <: Double](right: rightMin ~ rightMax)
      : Min[leftMin - rightMin, leftMin - rightMax] ~ Max[leftMax - rightMin, leftMax - rightMax] =

          left - right


      @annotation.targetName("minus2")
      infix def - [right <: Double & Singleton](right: right)
      : Min[leftMin - right, leftMax - right] ~ Max[leftMin - right, leftMax - right] =

          left - right


      @annotation.targetName("minus3")
      infix def - (right: Double): Double = left - right


      @annotation.targetName("divide")
      infix def / [right <: Double & Singleton](right: right)
      : Min[leftMin/right, leftMax/right] ~ Max[leftMin/right, leftMax/right] =

          left/right


      @annotation.targetName("divide2")
      infix def / [rightMin <: Double, rightMax <: Double](right: rightMin ~ rightMax)
      : Asym
         [rightMin*rightMax,
          Min4[leftMin/rightMin, leftMax/rightMin, leftMin/rightMax, leftMax/rightMax],
          -1.0/0.0]
        ~ Asym
           [rightMin*rightMax,
            Max4[leftMin/rightMin, leftMax/rightMin, leftMin/rightMax, leftMax/rightMax],
            1.0/0.0] =

          left/right


      @annotation.targetName("divide3")
      infix def / (right: Double): Double = left/right
