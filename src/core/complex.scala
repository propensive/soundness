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
  
  inline given quantityShow
      [UnitsType <: Measure]
      (using Decimalizer)
      : Show[Complex[Quantity[UnitsType]]] =
    
    new Show[Complex[Quantity[UnitsType]]]:
      def apply(value: Complex[Quantity[UnitsType]]): Text =
        t"${value.real.value} + ${value.imaginary.value}𝒾 ${Quantity.renderUnits(value.real.units)}"

  inline given add
      [ComponentType]
      (using addOperator: AddOperator[ComponentType, ComponentType])
      : AddOperator[Complex[ComponentType], Complex[ComponentType]] =
    new AddOperator[Complex[ComponentType], Complex[ComponentType]]:
      type Result = Complex[addOperator.Result]
    
      def add(left: Complex[ComponentType], right: Complex[ComponentType]): Complex[addOperator.Result] =
        Complex[addOperator.Result](left.real + right.real, left.imaginary + right.imaginary)

  inline given sub
      [ComponentType]
      (using subOperator: SubOperator[ComponentType, ComponentType])
      : SubOperator[Complex[ComponentType], Complex[ComponentType]] with
    type Result = Complex[subOperator.Result]
    
    def sub(left: Complex[ComponentType], right: Complex[ComponentType]): Complex[subOperator.Result] =
      Complex[subOperator.Result](left.real - right.real, left.imaginary - right.imaginary)
  
  inline given mul
      [ComponentType]
      (using mulOperator: MulOperator[ComponentType, ComponentType])
      (using addOperator: AddOperator[mulOperator.Result, mulOperator.Result])
      (using subOperator: SubOperator[mulOperator.Result, mulOperator.Result])
      : MulOperator[Complex[ComponentType], Complex[ComponentType]] with
    type Result = Complex[addOperator.Result | subOperator.Result]

    def mul
        (left: Complex[ComponentType], right: Complex[ComponentType])
        : Complex[addOperator.Result | subOperator.Result] =
      val ac: mulOperator.Result = left.real*right.real
      val bd: mulOperator.Result = left.imaginary*right.imaginary
      val ad: mulOperator.Result = left.real*right.imaginary
      val bc: mulOperator.Result = left.imaginary*right.real
    
      Complex(ac - bd, ad + bc)

  def polar
      [ComponentType]
      (modulus: ComponentType, argument: Double)
      (using mul: MulOperator[ComponentType, Double])
      : Complex[mul.Result] =
    Complex(modulus*math.cos(argument), modulus*math.sin(argument))

case class Complex[+ComponentType](real: ComponentType, imaginary: ComponentType):
  @targetName("add")
  inline def +
      [ComponentType2](right: Complex[ComponentType2])
      (using addOperator: AddOperator[ComponentType, ComponentType2])
      : Complex[addOperator.Result] =
    Complex(this.real + right.real, this.imaginary + right.imaginary)

  @targetName("sub")
  inline def -
      [ComponentType2](right: Complex[ComponentType2])
      (using subOperator: SubOperator[ComponentType, ComponentType2])
      : Complex[subOperator.Result] =
    Complex(this.real - right.real, this.imaginary - right.imaginary)
  
  @targetName("mul")
  inline def *
      [ComponentType2](right: Complex[ComponentType2])
      (using mulOperator: MulOperator[ComponentType, ComponentType2])
      (using addOperator: AddOperator[mulOperator.Result, mulOperator.Result])
      (using subOperator: SubOperator[mulOperator.Result, mulOperator.Result])
      : Complex[subOperator.Result | addOperator.Result] =

    val ac: mulOperator.Result = real*right.real
    val bd: mulOperator.Result = imaginary*right.imaginary
    val ad: mulOperator.Result = real*right.imaginary
    val bc: mulOperator.Result = imaginary*right.real
    
    Complex(ac - bd, ad + bc)
  
  @targetName("div")
  inline def /
      [ComponentType2](right: Complex[ComponentType2])
      (using mulOperator: MulOperator[ComponentType, ComponentType2])
      (using mulOperator2: MulOperator[ComponentType2, ComponentType2])
      (using addOperator: AddOperator[mulOperator.Result, mulOperator.Result])
      (using addOperator2: AddOperator[mulOperator2.Result, mulOperator2.Result])
      (using subOperator: SubOperator[mulOperator.Result, mulOperator.Result])
      (using divOperator: DivOperator[subOperator.Result | addOperator.Result, addOperator2.Result])
      : Complex[divOperator.Result] =

    val ac: mulOperator.Result = mulOperator.mul(real, right.real)
    val bd: mulOperator.Result = mulOperator.mul(imaginary, right.imaginary)
    val ad: mulOperator.Result = mulOperator.mul(real, right.imaginary)
    val bc: mulOperator.Result = mulOperator.mul(imaginary, right.real)

    val divisor = addOperator2.add(mulOperator2.mul(right.real, right.real), mulOperator2.mul(right.imaginary, right.imaginary))
    
    Complex((ac + bd)/divisor, (bc - ad)/divisor)

  inline def argument
      (using mul: MulOperator[ComponentType, ComponentType])
      (using add: AddOperator[mul.Result, mul.Result])
      (using sqrt: SquareRoot[add.Result])
      (using div: DivOperator[ComponentType, sqrt.Result])
      (using div.Result =:= Double)
      : Double =
    scala.math.atan2(imaginary/modulus, real/modulus)

  inline def modulus
      (using mul: MulOperator[ComponentType, ComponentType])
      (using add: AddOperator[mul.Result, mul.Result])
      (using squareRoot: SquareRoot[add.Result])
      : squareRoot.Result =
    squareRoot.sqrt(real*real + imaginary*imaginary)

  inline def sqrt
      (using mul: MulOperator[ComponentType, ComponentType])
      (using add: AddOperator[mul.Result, mul.Result])
      (using sqrt: SquareRoot[add.Result])
      (using div: DivOperator[ComponentType, sqrt.Result])
      (using div.Result =:= Double)
      (using sqrt2: SquareRoot[sqrt.Result])
      (using mul2: MulOperator[sqrt2.Result, Double])
      : Complex[mul2.Result] =
    Complex.polar(modulus.sqrt, argument/2.0)

  @targetName("conjugate")
  inline def unary_~(using neg: NegOperator[ComponentType]): Complex[ComponentType | neg.Result] =
    Complex(real, -imaginary)
  
  @targetName("neg")
  inline def unary_-(using neg: NegOperator[ComponentType]): Complex[ComponentType | neg.Result] =
    Complex(-real, -imaginary)

