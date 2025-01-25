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
import prepositional.*
import quantitative.*
import spectacular.*
import symbolism.*

import scala.annotation.*

object Complex:
  given show: [ComponentType: Showable] => Complex[ComponentType] is Showable = complex =>
   t"${complex.real.show} + ${complex.imaginary.show}𝒾"

  inline given [UnitsType <: Measure] => Decimalizer => Complex[Quantity[UnitsType]] is Showable:
    def text(value: Complex[Quantity[UnitsType]]): Text =
      t"${value.real.value} + ${value.imaginary.value}𝒾 ${Quantity.renderUnits(value.real.units)}"

  inline given addable: [ComponentType: Addable by ComponentType as addable]
  =>    Complex[ComponentType] is Addable by Complex[ComponentType] =
    new Addable.Basic[Complex[ComponentType], Complex[ComponentType], Complex[addable.Result]]
      ({ (left, right) =>
           Complex[addable.Result](left.real + right.real, left.imaginary + right.imaginary) })

  inline given subtractable: [ComponentType: Subtractable by ComponentType as subtractable]
  =>   Complex[ComponentType] is Subtractable by Complex[ComponentType] =
    new Subtractable.Basic
         [Complex[ComponentType], Complex[ComponentType], Complex[subtractable.Result]]
      ({ (left, right) =>
           Complex[subtractable.Result](left.real - right.real, left.imaginary - right.imaginary) })

  inline given multiplicable: [ComponentType]
  =>   (multiplication: ComponentType is Multiplicable by ComponentType,
        addition:       multiplication.Result is Addable by multiplication.Result,
        subtraction:    multiplication.Result is Subtractable by multiplication.Result)
  =>    Complex[ComponentType] is Multiplicable by Complex[ComponentType] =
    Multiplicable[Complex[ComponentType],
                  Complex[ComponentType],
                  Complex[addition.Result | subtraction.Result]]:
      (left, right) =>
        Complex
         (left.real*right.real - left.imaginary*right.imaginary,
          left.real*right.imaginary + left.imaginary*right.real)

  def polar[ComponentType: Multiplicable by Double as multiplication]
     (modulus: ComponentType, argument: Double)
  :     Complex[multiplication.Result] =
    Complex(modulus*math.cos(argument), modulus*math.sin(argument))

case class Complex[ComponentType](real: ComponentType, imaginary: ComponentType):
  @targetName("add")
  inline infix def + [ComponentType2](right: Complex[ComponentType2])
     (using addition: ComponentType is Addable by ComponentType2)
  :     Complex[addition.Result] =
    Complex(this.real + right.real, this.imaginary + right.imaginary)

  @targetName("sub")
  inline infix def - [ComponentType2](right: Complex[ComponentType2])
     (using subtraction: ComponentType is Subtractable by ComponentType2)
  :     Complex[subtraction.Result] =
    Complex(this.real - right.real, this.imaginary - right.imaginary)

  @targetName("mul")
  inline infix def * [ComponentType2](right: Complex[ComponentType2])
     (using multiplication: ComponentType is Multiplicable by ComponentType2,
            addition:      multiplication.Result is Addable by multiplication.Result,
            subtraction:    multiplication.Result is Subtractable by multiplication.Result)
  :     Complex[subtraction.Result | addition.Result] =

    Complex
     (real*right.real - imaginary*right.imaginary, real*right.imaginary + imaginary*right.real)

  @targetName("div")
  inline infix def / [ComponentType2](right: Complex[ComponentType2])
     (using multiplication:  ComponentType is Multiplicable by ComponentType2,
            multiplication2: ComponentType2 is Multiplicable by ComponentType2,
            addition:     multiplication.Result is Addable by multiplication.Result,
            addition2:      multiplication2.Result is Addable by multiplication2.Result,
            subtraction:    multiplication.Result is Subtractable by multiplication.Result,
            divisible:      subtraction.Result | addition.Result is Divisible by addition2.Result)
  :     Complex[divisible.Result] =

    val divisor = right.real*right.real + right.imaginary*right.imaginary

    Complex
     ((real*right.real + imaginary*right.imaginary)/divisor,
      (imaginary*right.real - real*right.imaginary)/divisor)

  inline def argument
     (using multiplication: ComponentType is Multiplicable by ComponentType,
            addition:      multiplication.Result is Addable by multiplication.Result,
            sqrt:        addition.Result is Rootable[2],
            division:      ComponentType is Divisible by sqrt.Result,
            equality:      division.Result =:= Double)
  :     Double =

    scala.math.atan2(imaginary/modulus, real/modulus)

  inline def modulus
     (using multiplication: ComponentType is Multiplicable by ComponentType,
            addition:      multiplication.Result is Addable by multiplication.Result,
            squareRoot:    addition.Result is Rootable[2])
  :     squareRoot.Result =
    squareRoot.root(real*real + imaginary*imaginary)

  inline def sqrt
     (using multiplication:  ComponentType is Multiplicable by ComponentType,
            addition:     multiplication.Result is Addable by multiplication.Result,
            sqrt:         addition.Result is Rootable[2],
            division:     ComponentType is Divisible by sqrt.Result,
            equality:     division.Result =:= Double,
            sqrt2:        sqrt.Result is Rootable[2],
            multiplication2: sqrt2.Result is Multiplicable by Double)
  :     Complex[multiplication2.Result] =
    Complex.polar(modulus.sqrt, argument/2.0)

  @targetName("conjugate")
  inline def unary_~(using neg: ComponentType is Negatable): Complex[ComponentType | neg.Result] =
    Complex(real, -imaginary)

  @targetName("neg")
  inline def unary_-(using neg: ComponentType is Negatable): Complex[ComponentType | neg.Result] =
    Complex(-real, -imaginary)
