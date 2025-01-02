/*
    Baroque, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package baroque

import anticipation.*
import gossamer.*
import quantitative.*
import spectacular.*
import symbolism.*

import scala.annotation.*

val I: Complex[Double] = Complex(0.0, 1.0)

object Complex:
  given show[ComponentType: Show]: Show[Complex[ComponentType]] = complex =>
   t"${complex.real.show} + ${complex.imaginary.show}𝒾"

  inline given [UnitsType <: Measure](using Decimalizer) => Show[Complex[Quantity[UnitsType]]]:
    def text(value: Complex[Quantity[UnitsType]]): Text =
      t"${value.real.value} + ${value.imaginary.value}𝒾 ${Quantity.renderUnits(value.real.units)}"

  inline given [ComponentType: Addable[ComponentType] as addable]
      => Complex[ComponentType] is Addable[Complex[ComponentType]] as addable =
    new Addable.Basic[Complex[ComponentType], Complex[ComponentType], Complex[addable.Result]]
      ({ (left, right) =>
           Complex[addable.Result](left.real + right.real, left.imaginary + right.imaginary) })

  inline given [ComponentType: Subtractable[ComponentType] as subtractable]
      => Complex[ComponentType] is Subtractable[Complex[ComponentType]] as subtractable =
    new Subtractable.Basic[Complex[ComponentType], Complex[ComponentType], Complex[subtractable.Result]]
      ({ (left, right) =>
           Complex[subtractable.Result](left.real - right.real, left.imaginary - right.imaginary) })

  inline given [ComponentType]
     (using multiplication: ComponentType is Multiplicable[ComponentType],
            addition:       multiplication.Result is Addable[multiplication.Result],
            subtraction:    multiplication.Result is Subtractable[multiplication.Result])
        => Complex[ComponentType] is Multiplicable[Complex[ComponentType]]:

    type Result = Complex[addition.Result | subtraction.Result]

    def multiply(left: Complex[ComponentType], right: Complex[ComponentType])
            : Complex[addition.Result | subtraction.Result] =

      Complex
       (left.real*right.real - left.imaginary*right.imaginary,
        left.real*right.imaginary + left.imaginary*right.real)

  def polar[ComponentType: Multiplicable[Double] as multiplication]
     (modulus: ComponentType, argument: Double)
          : Complex[multiplication.Result] =
    Complex(modulus*math.cos(argument), modulus*math.sin(argument))

case class Complex[ComponentType](real: ComponentType, imaginary: ComponentType):
  @targetName("add")
  inline infix def + [ComponentType2](right: Complex[ComponentType2])
     (using addition: ComponentType is Addable[ComponentType2])
          : Complex[addition.Result] =
    Complex(this.real + right.real, this.imaginary + right.imaginary)

  @targetName("sub")
  inline infix def - [ComponentType2](right: Complex[ComponentType2])
     (using subtraction: ComponentType is Subtractable[ComponentType2])
          : Complex[subtraction.Result] =
    Complex(this.real - right.real, this.imaginary - right.imaginary)

  @targetName("mul")
  inline infix def * [ComponentType2](right: Complex[ComponentType2])
     (using multiplication: ComponentType is Multiplicable[ComponentType2],
            addition:       multiplication.Result is Addable[multiplication.Result],
            subtraction:    multiplication.Result is Subtractable[multiplication.Result])
          : Complex[subtraction.Result | addition.Result] =

    Complex(real*right.real - imaginary*right.imaginary, real*right.imaginary + imaginary*right.real)

  @targetName("div")
  inline infix def / [ComponentType2](right: Complex[ComponentType2])
     (using multiplication:  ComponentType is Multiplicable[ComponentType2],
            multiplication2: ComponentType2 is Multiplicable[ComponentType2],
            addition:        multiplication.Result is Addable[multiplication.Result],
            addition2:       multiplication2.Result is Addable[multiplication2.Result],
            subtraction:     multiplication.Result is Subtractable[multiplication.Result],
            divisible:       subtraction.Result | addition.Result is Divisible[addition2.Result])
          : Complex[divisible.Result] =

    val divisor = right.real*right.real + right.imaginary*right.imaginary

    Complex
     ((real*right.real + imaginary*right.imaginary)/divisor,
      (imaginary*right.real - real*right.imaginary)/divisor)

  inline def argument
     (using multiplication: ComponentType is Multiplicable[ComponentType],
            addition:       multiplication.Result is Addable[multiplication.Result],
            sqrt:           Rootable[2, addition.Result],
            division:       ComponentType is Divisible[sqrt.Result],
            equality:       division.Result =:= Double)
          : Double =

    scala.math.atan2(imaginary/modulus, real/modulus)

  inline def modulus
     (using multiplication: ComponentType is Multiplicable[ComponentType],
            addition:       multiplication.Result is Addable[multiplication.Result],
            squareRoot:     Rootable[2, addition.Result])
          : squareRoot.Result =
    squareRoot.root(real*real + imaginary*imaginary)

  inline def sqrt
     (using multiplication:  ComponentType is Multiplicable[ComponentType],
            addition:        multiplication.Result is Addable[multiplication.Result],
            sqrt:            Rootable[2, addition.Result],
            division:        ComponentType is Divisible[sqrt.Result],
            equality:        division.Result =:= Double,
            sqrt2:           Rootable[2, sqrt.Result],
            multiplication2: sqrt2.Result is Multiplicable[Double])
          : Complex[multiplication2.Result] =
    Complex.polar(modulus.sqrt, argument/2.0)

  @targetName("conjugate")
  inline def unary_~(using neg: ComponentType is Negatable): Complex[ComponentType | neg.Result] =
    Complex(real, -imaginary)

  @targetName("neg")
  inline def unary_-(using neg: ComponentType is Negatable): Complex[ComponentType | neg.Result] =
    Complex(-real, -imaginary)
