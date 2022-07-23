package cardinality

import compiletime.ops.double.*, compiletime.ops.int.ToDouble
import scala.util.FromDigits
import annotation.*

import language.experimental.genericNumberLiterals

object NumericRange:
  @targetName("Range")
  opaque infix type ~[D1 <: Double, D2 <: Double] = Double

  def apply[D1 <: Double, D2 <: Double](value: Double): D1 ~ D2  = value

  @targetName("Range")
  object `~`:
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
           : (Min[Min[D1*E1, D1*E2], Min[D2*E2, D2*E1]]) ~ (Max[Max[D1*E1, D1*E2], Max[D2*E2, D2*E1]]) =
        left*right
      
      @targetName("times2")
      def *[E <: Double & Singleton](right: E): Min[D1*E, D2*E] ~ Max[D1*E, D2*E] = left*right
      
      @targetName("times2a")
      def *[E <: Int & Singleton](right: E): Min[D1*ToDouble[E], D2*ToDouble[E]] ~ Max[D1*ToDouble[E], D2*ToDouble[E]] = left*right

      @targetName("times3")
      def *(right: Double): Double = left*right
    
      @targetName("minus2")
      def -[E <: Double & Singleton](right: E): Min[D1 - E, D2 - E] ~ Max[D1 - E, D2 - E] = left - right

      @targetName("minus3")
      def -(right: Double): Double = left - right

export NumericRange.`~`