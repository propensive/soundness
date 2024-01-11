/*
    Hypotenuse, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hypotenuse

import anticipation.*

import scala.annotation.*

inline def cos(double: Double): Double = math.cos(double)
inline def acos(double: Double): Double = math.acos(double)
inline def cosh(double: Double): Double = math.cosh(double)
inline def sin(double: Double): Double = math.sin(double)
inline def asin(double: Double): Double = math.asin(double)
inline def sinh(double: Double): Double = math.sinh(double)
inline def tan(double: Double): Double = math.tan(double)
inline def atan(double: Double): Double = math.atan(double)
inline def hypot(first: Double, second: Double): Double = math.hypot(first, second)

inline def exp(double: Double): Double = math.exp(double)
inline def expm1(double: Double): Double = math.expm1(double)
inline def log(double: Double): Double = math.log(double)
inline def log10(double: Double): Double = math.log10(double)
inline def log1p(double: Double): Double = math.log1p(double)

extension (float: Float)
  inline def abs: Float = math.abs(float)
  inline def sqrt: Float = math.sqrt(float).toFloat
  inline def cbrt: Float = math.cbrt(float).toFloat
  inline def ceiling: Float = math.ceil(float).toFloat
  inline def floor: Float = math.floor(float).toFloat
  inline def exponent: Int = math.getExponent(float)
  inline def increment: Float = math.nextUp(float)
  inline def decrement: Float = math.nextDown(float)
  inline def round: Long = math.round(float)
  inline def scalb(scale: Int): Float = math.scalb(float, scale)
  inline def signum: -1.0F | 0.0F | 1.0F = math.signum(float).asInstanceOf[-1.0F | 0.0F | 1.0F]
  inline def ulp: Float = math.ulp(float)
  inline def bits: Long = java.lang.Float.floatToIntBits(float)
  inline def rawBits: Long = java.lang.Float.floatToRawIntBits(float)
  inline def finite: Boolean = float.isFinite
  inline def infinite: Boolean = float.isInfinite
  inline def nan: Boolean = float.isNaN
  
  @targetName("power")
  inline infix def **(exponent: Double): Float = math.pow(float, exponent).toFloat

extension (double: Double)
  inline def abs: Double = math.abs(double)
  inline def sqrt: Double = math.sqrt(double)
  inline def cbrt: Double = math.cbrt(double)
  inline def ceiling: Double = math.ceil(double)
  inline def floor: Double = math.floor(double)
  inline def exponent: Int = math.getExponent(double)
  inline def increment: Double = math.nextUp(double)
  inline def decrement: Double = math.nextDown(double)
  inline def round: Long = math.round(double)
  inline def scalb(scale: Int): Double = math.scalb(double, scale)
  inline def signum: -1.0 | 0.0 | 1.0 = math.signum(double).asInstanceOf[-1.0 | 0.0 | 1.0]
  inline def ulp: Double = math.ulp(double)
  inline def bits: Long = java.lang.Double.doubleToLongBits(double)
  inline def rawBits: Long = java.lang.Double.doubleToRawLongBits(double)
  inline def finite: Boolean = double.isFinite
  inline def infinite: Boolean = double.isInfinite
  inline def nan: Boolean = double.isNaN

  @targetName("power")
  inline infix def **(exponent: Double): Double = math.pow(double, exponent)

extension (int: Int)
  inline def long: Long = int.toLong
  inline def abs: Int = math.abs(int)
  inline def decrement: Int = math.decrementExact(int)
  inline def increment: Int = math.incrementExact(int)
  
  @targetName("power")
  inline infix def **(exponent: Double): Double = math.pow(int.toDouble, exponent)

  inline def bits: Int = java.lang.Long.bitCount(int)
  inline def highestBit: Int = java.lang.Integer.highestOneBit(int)
  inline def lowestBit: Int = java.lang.Integer.lowestOneBit(int)
  inline def leadingZeros: Int = java.lang.Integer.numberOfLeadingZeros(int)
  inline def trailingZeros: Int = java.lang.Integer.numberOfTrailingZeros(int)
  inline def reverseBits: Int = java.lang.Integer.reverse(int)
  inline def reverseBytes: Int = java.lang.Integer.reverseBytes(int)
  inline def rotateLeft(bits: Int): Int = java.lang.Integer.rotateLeft(int, bits)
  inline def rotateRight(bits: Int): Int = java.lang.Integer.rotateRight(int, bits)
  inline def octal: Text = java.lang.Integer.toOctalString(int).nn.tt
  inline def hex: Text = java.lang.Integer.toHexString(int).nn.tt
  inline def binary: Text = java.lang.Integer.toBinaryString(int).nn.tt

extension (long: Long)
  inline def abs: Long = math.abs(long)
  inline def decrement: Long = math.decrementExact(long)
  inline def increment: Long = math.incrementExact(long)
  inline def bits: Int = java.lang.Long.bitCount(long)
  inline def highestBit: Long = java.lang.Long.highestOneBit(long)
  inline def lowestBit: Long = java.lang.Long.lowestOneBit(long)
  inline def leadingZeros: Int = java.lang.Long.numberOfLeadingZeros(long)
  inline def trailingZeros: Int = java.lang.Long.numberOfTrailingZeros(long)
  inline def reverseBits: Long = java.lang.Long.reverse(long)
  inline def reverseBytes: Long = java.lang.Long.reverseBytes(long)
  inline def rotateLeft(bits: Int): Long = java.lang.Long.rotateLeft(long, bits)
  inline def rotateRight(bits: Int): Long = java.lang.Long.rotateRight(long, bits)
  inline def octal: Text = java.lang.Long.toOctalString(long).nn.tt
  inline def hex: Text = java.lang.Long.toHexString(long).nn.tt
  inline def binary: Text = java.lang.Long.toBinaryString(long).nn.tt
  
  @targetName("power")
  inline infix def **(exponent: Double): Double = math.pow(long.toDouble, exponent)


def erf(value: Double): Double =
  val a = 0.254829592
  val b = -0.284496736
  val c = 1.421413741
  val d = -1.453152027
  val e = 1.061405429
  val p = 0.3275911
  
  val x = math.abs(value)
  val t = 1.0/(1.0 + p*x)
  val y = 1 - (((((e*t + d)*t) + c)*t + b)*t + a)*t*math.exp(-x*x)
  
  math.signum(value)*y

final val π = math.Pi
final val pi = math.Pi
final val e = math.E
final val eulerNumber = math.E
final val φ = (1.0 + 5.sqrt)/2.0
final val goldenRatio = φ