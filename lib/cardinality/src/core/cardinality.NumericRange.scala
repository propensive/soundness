                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                    ╭───╮                                         ┃
┃  ╭─────────╮                                       │   │                                         ┃
┃  │   ╭─────╯╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮  ┃
┃  │   ╰─────╮│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯  ┃
┃  ╰─────╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ├╌╯╌─╯╰─╌ ╰───╮╰─╌ ╰───╮  ┃
┃  ╭─────╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╌   │╭───╌   │  ┃
┃  ╰─────────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯  ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0. © Copyright 2023-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        http://www.apache.org/licenses/LICENSE-2.0                                                ┃
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
  opaque infix type ~ [MinValueType <: Double, MaxValueType <: Double] = Double

  def apply[MinValueType <: Double, MaxValueType <: Double](value: Double)
  :     MinValueType ~ MaxValueType =
    value

  @annotation.targetName("Range")
  object `~`:
    given comparable[MinValueType <: Double & Singleton, MaxValueType <: Double & Singleton]
       (using min: ValueOf[MinValueType], max: ValueOf[MaxValueType])
    :     TypeTest[Double, MinValueType ~ MaxValueType] =

      value =>
        if value >= min.value && value <= max.value
        then Some(value.asInstanceOf[(MinValueType ~ MaxValueType) & value.type])
        else None

    class RangeParser[MinValueType <: Double, MaxValueType <: Double]
    extends FromDigits.Decimal[MinValueType ~ MaxValueType]:
      def fromDigits(digits: String): Double = apply(digits.toDouble)

    given cardinality[MinValueType <: Double, MaxValueType <: Double]
    :     RangeParser[MinValueType, MaxValueType] with
      override inline def fromDigits(digits: String): MinValueType ~ MaxValueType =
        ${Cardinality('digits)}

    extension [LeftMinType <: Double, LeftMaxType <: Double](left: LeftMinType ~ LeftMaxType)
      def double: Double = left

      @annotation.targetName("add")
      infix def + [RightMinType <: Double, RightMaxType <: Double]
         (right: RightMinType ~ RightMaxType)
      :     (LeftMinType + RightMinType) ~ (LeftMaxType + RightMaxType) =
        left + right

      @annotation.targetName("add2")
      infix def + [E <: Double & Singleton](right: E)
      :     (LeftMinType + right.type) ~ (LeftMaxType + right.type) =
        left + right

      @annotation.targetName("add3")
      infix def + (right: Double): Double = left + right

      @annotation.targetName("times")
      infix def * [RightMinType <: Double, RightMaxType <: Double]
         (right: RightMinType ~ RightMaxType)
      :     (Min4
              [LeftMinType*RightMinType,
               LeftMinType*RightMaxType,
               LeftMaxType*RightMaxType,
               LeftMaxType*RightMinType]) ~ (Max4
                                              [LeftMinType*RightMinType,
                                               LeftMinType*RightMaxType,
                                               LeftMaxType*RightMaxType,
                                               LeftMaxType*RightMinType]) =

        left*right

      @annotation.targetName("times2")
      infix def * [RightType <: Double & Singleton](right: RightType)
      :     Min
             [LeftMinType*RightType,
              LeftMaxType*RightType] ~ Max
                                        [LeftMinType*RightType,
                                         LeftMaxType*RightType] =

        left*right

      @annotation.targetName("times3")
      infix def * (right: Double): Double = left*right

      @annotation.targetName("minus")
      infix def - [RightMinType <: Double, RightMaxType <: Double]
         (right: RightMinType ~ RightMaxType)
      :     Min
             [LeftMinType - RightMinType,
              LeftMinType - RightMaxType] ~ Max
                                             [LeftMaxType - RightMinType,
                                              LeftMaxType - RightMaxType] =
        left - right

      @annotation.targetName("minus2")
      infix def - [RightType <: Double & Singleton](right: RightType)
      :     Min
             [LeftMinType - RightType,
              LeftMaxType - RightType] ~ Max
                                          [LeftMinType - RightType,
                                           LeftMaxType - RightType] =

        left - right

      @annotation.targetName("minus3")
      infix def - (right: Double): Double = left - right

      @annotation.targetName("divide")
      infix def / [RightType <: Double & Singleton](right: RightType)
      :     Min
             [LeftMinType/RightType,
              LeftMaxType/RightType] ~ Max
                                        [LeftMinType/RightType,
                                         LeftMaxType/RightType] =

        left/right

      @annotation.targetName("divide2")
      infix def / [RightMinType <: Double, RightMaxType <: Double]
         (right: RightMinType ~ RightMaxType)
      :     Asym
             [RightMinType*RightMaxType,
              Min4
               [LeftMinType/RightMinType,
                LeftMaxType/RightMinType,
                LeftMinType/RightMaxType,
                LeftMaxType/RightMaxType],
              -1.0/0.0] ~ Asym
                           [RightMinType*RightMaxType,
                            Max4
                             [LeftMinType/RightMinType,
                              LeftMaxType/RightMinType,
                              LeftMinType/RightMaxType,
                              LeftMaxType/RightMaxType],
                            1.0/0.0] =
        left/right

      @annotation.targetName("divide3")
      infix def / (right: Double): Double = left/right
