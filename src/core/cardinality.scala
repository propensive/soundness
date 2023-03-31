/*
    Cardinality, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cardinality

import compiletime.ops.double.*
import scala.util.FromDigits
import scala.reflect.TypeTest
import annotation.*

import language.experimental.genericNumberLiterals

type Asym[V <: Double, T <: Double, F <: Double] <: Double = (V > 0.0) match
  case true  => T
  case false => F

type Min4[A <: Double, B <: Double, C <: Double, D <: Double] = Min[Min[A, B], Min[C, D]]
type Max4[A <: Double, B <: Double, C <: Double, D <: Double] = Max[Max[A, B], Max[C, D]]

extension (value: Double)
  def force[D1 <: Double, D2 <: Double]: D1 ~ D2 = value.asInstanceOf[D1 ~ D2]

object NumericRange:
  @targetName("Range")
  opaque infix type ~[D1 <: Double, D2 <: Double] = Double

  def apply[D1 <: Double, D2 <: Double](value: Double): D1 ~ D2  = value

  @targetName("Range")
  object `~`:
    given comparable
        [D1 <: Double & Singleton, D2 <: Double & Singleton]
        (using d1: ValueOf[D1], d2: ValueOf[D2])
        : TypeTest[Double, D1 ~ D2] =
      value =>
        if value >= d1.value && value <= d2.value
        then Some(value.asInstanceOf[(D1 ~ D2) & value.type])
        else None

    class Fd[D1 <: Double, D2 <: Double] extends FromDigits.Decimal[D1 ~ D2]:
      def fromDigits(digits: String): Double = apply(digits.toDouble)

    given cardinality[D1 <: Double, D2 <: Double]: Fd[D1, D2] with
      override inline def fromDigits(digits: String): D1 ~ D2 = ${CardinalityMacro('digits)}
    
    extension [D1 <: Double, D2 <: Double](left: D1 ~ D2)
      def double: Double = left

      @targetName("add")
      def +[E1 <: Double, E2 <: Double](right: E1 ~ E2): (D1 + E1) ~ (D2 + E2) = left + right
      
      @targetName("add2")
      def +[E <: Double & Singleton](right: E): (D1 + right.type) ~ (D2 + right.type) = left + right

      @targetName("add3")
      def +(right: Double): Double = left + right

      @targetName("times")
      def *[E1 <: Double, E2 <: Double](right: E1 ~ E2)
           : (Min4[D1*E1, D1*E2, D2*E2, D2*E1]) ~ (Max4[D1*E1, D1*E2, D2*E2, D2*E1]) =
        left*right
      
      @targetName("times2")
      def *[E <: Double & Singleton](right: E): Min[D1*E, D2*E] ~ Max[D1*E, D2*E] = left*right
      
      @targetName("times3")
      def *(right: Double): Double = left*right

      @targetName("minus")
      def -
          [E1 <: Double, E2 <: Double]
          (right: E1 ~ E2)
          : Min[D1 - E1, D1 - E2] ~ Max[D2 - E1, D2 - E2] =
        left - right

      @targetName("minus2")
      def -[E <: Double & Singleton](right: E): Min[D1 - E, D2 - E] ~ Max[D1 - E, D2 - E] =
        left - right

      @targetName("minus3")
      def -(right: Double): Double = left - right

      @targetName("divide")
      def /[E <: Double & Singleton](right: E): Min[D1/E, D2/E] ~ Max[D1/E, D2/E] =
        left/right
      
      @targetName("divide2")
      def /
          [E1 <: Double, E2 <: Double](right: E1 ~ E2)
          : Asym[E1*E2, Min4[D1/E1, D2/E1, D1/E2, D2/E2], -1.0/0.0] ~
              Asym[E1*E2, Max4[D1/E1, D2/E1, D1/E2, D2/E2], 1.0/0.0] =
        left/right
      
      @targetName("divide3")
      def /(right: Double): Double = left/right

export NumericRange.`~`
