/*
    Baroque, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import spectacular.*
import quantitative.*
import anticipation.*
import symbolism.*

import scala.annotation.*

val I: Complex[Double] = Complex(0.0, 1.0)

object Complex:
  given show[ComponentType: Show]: Show[Complex[ComponentType]] = complex =>
   t"${complex.real.show} + ${complex.imaginary.show}𝒾"

  inline given [UnitsType <: Measure](using Decimalizer) => Show[Complex[Quantity[UnitsType]]]:
    def text(value: Complex[Quantity[UnitsType]]): Text =
      t"${value.real.value} + ${value.imaginary.value}𝒾 ${Quantity.renderUnits(value.real.units)}"

  inline given [ComponentType: Addable[ComponentType] as addable] => Complex[ComponentType] is Addable[Complex[ComponentType]] as addable =
    new Addable.Basic[Complex[ComponentType], Complex[ComponentType], Complex[addable.Result]](
      (left, right) => Complex[addable.Result](left.real + right.real, left.imaginary + right.imaginary)
    )

  inline given [ComponentType: Subtractable[ComponentType] as subtractable] => Complex[ComponentType] is Subtractable[Complex[ComponentType]] as subtractable =
    new Subtractable.Basic[Complex[ComponentType], Complex[ComponentType], Complex[subtractable.Result]](
      (left, right) => Complex[subtractable.Result](left.real - right.real, left.imaginary - right.imaginary)
    )

  inline given [ComponentType]
      (using mulOperator: ComponentType is Multiplicable[ComponentType])
      (using addOperator: mulOperator.Result is Addable[mulOperator.Result])
      (using subOperator: mulOperator.Result is Subtractable[mulOperator.Result]) => Complex[ComponentType] is Multiplicable[Complex[ComponentType]]:

    type Result = Complex[addOperator.Result | subOperator.Result]

    def multiply(left: Complex[ComponentType], right: Complex[ComponentType])
            : Complex[addOperator.Result | subOperator.Result] =
      val ac: mulOperator.Result = left.real*right.real
      val bd: mulOperator.Result = left.imaginary*right.imaginary
      val ad: mulOperator.Result = left.real*right.imaginary
      val bc: mulOperator.Result = left.imaginary*right.real

      Complex(ac - bd, ad + bc)

  def polar[ComponentType](modulus: ComponentType, argument: Double)
      (using mul: ComponentType is Multiplicable[Double])
          : Complex[mul.Result] =
    Complex(modulus*math.cos(argument), modulus*math.sin(argument))

case class Complex[ComponentType](real: ComponentType, imaginary: ComponentType):
  @targetName("add")
  inline infix def + [ComponentType2](right: Complex[ComponentType2])
      (using addOperator: ComponentType is Addable[ComponentType2])
          : Complex[addOperator.Result] =
    Complex(this.real + right.real, this.imaginary + right.imaginary)

  @targetName("sub")
  inline infix def - [ComponentType2](right: Complex[ComponentType2])
      (using subOperator: ComponentType is Subtractable[ComponentType2])
          : Complex[subOperator.Result] =
    Complex(this.real - right.real, this.imaginary - right.imaginary)

  @targetName("mul")
  inline infix def * [ComponentType2](right: Complex[ComponentType2])
      (using mulOperator: ComponentType is Multiplicable[ComponentType2])
      (using addOperator: mulOperator.Result is Addable[mulOperator.Result])
      (using subOperator: mulOperator.Result is Subtractable[mulOperator.Result])
          : Complex[subOperator.Result | addOperator.Result] =

    val ac: mulOperator.Result = real*right.real
    val bd: mulOperator.Result = imaginary*right.imaginary
    val ad: mulOperator.Result = real*right.imaginary
    val bc: mulOperator.Result = imaginary*right.real

    Complex(ac - bd, ad + bc)

  @targetName("div")
  inline infix def / [ComponentType2](right: Complex[ComponentType2])
      (using mulOperator: ComponentType is Multiplicable[ComponentType2])
      (using mulOperator2: ComponentType2 is Multiplicable[ComponentType2])
      (using addOperator: mulOperator.Result is Addable[mulOperator.Result])
      (using addOperator2: mulOperator2.Result is Addable[mulOperator2.Result])
      (using subOperator: mulOperator.Result is Subtractable[mulOperator.Result])
      (using divisible: (subOperator.Result | addOperator.Result) is Divisible[addOperator2.Result])
          : Complex[divisible.Result] =

    val ac: mulOperator.Result = mulOperator.multiply(real, right.real)
    val bd: mulOperator.Result = mulOperator.multiply(imaginary, right.imaginary)
    val ad: mulOperator.Result = mulOperator.multiply(real, right.imaginary)
    val bc: mulOperator.Result = mulOperator.multiply(imaginary, right.real)

    val divisor = addOperator2.add(mulOperator2.multiply(right.real, right.real), mulOperator2.multiply(right.imaginary, right.imaginary))

    Complex((ac + bd)/divisor, (bc - ad)/divisor)

  inline def argument
      (using mul: ComponentType is Multiplicable[ComponentType])
      (using add: mul.Result is Addable[mul.Result])
      (using sqrt: Rootable[2, add.Result])
      (using div: ComponentType is Divisible[sqrt.Result])
      (using div.Result =:= Double)
          : Double =
    scala.math.atan2(imaginary/modulus, real/modulus)

  inline def modulus
      (using mul: ComponentType is Multiplicable[ComponentType])
      (using add: mul.Result is Addable[mul.Result])
      (using squareRoot: Rootable[2, add.Result])
          : squareRoot.Result =
    squareRoot.root(real*real + imaginary*imaginary)

  inline def sqrt
      (using mul: ComponentType is Multiplicable[ComponentType])
      (using add: mul.Result is Addable[mul.Result])
      (using sqrt: Rootable[2, add.Result])
      (using div: ComponentType is Divisible[sqrt.Result])
      (using div.Result =:= Double)
      (using sqrt2: Rootable[2, sqrt.Result])
      (using mul2: sqrt2.Result is Multiplicable[Double])
          : Complex[mul2.Result] =
    Complex.polar(modulus.sqrt, argument/2.0)

  @targetName("conjugate")
  inline def unary_~(using neg: ComponentType is Negatable): Complex[ComponentType | neg.Result] =
    Complex(real, -imaginary)

  @targetName("neg")
  inline def unary_-(using neg: ComponentType is Negatable): Complex[ComponentType | neg.Result] =
    Complex(-real, -imaginary)
