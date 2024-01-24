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

  given add
      [ComponentType, ComponentType2]
      (using addOperator: AddOperator[ComponentType, ComponentType2])
      : AddOperator[Complex[ComponentType], Complex[ComponentType2]] =
    new AddOperator[Complex[ComponentType], Complex[ComponentType2]]:
      type Result = Complex[addOperator.Result]
    
      def add(left: Complex[ComponentType], right: Complex[ComponentType2]): Complex[addOperator.Result] =
        Complex[addOperator.Result](left.real + right.real, left.imaginary + right.imaginary)

  given sub
      [ComponentType, ComponentType2]
      (using subOperator: SubOperator[ComponentType, ComponentType2])
      : SubOperator[Complex[ComponentType], Complex[ComponentType2]] with
    type Result = Complex[subOperator.Result]
    
    def sub(left: Complex[ComponentType], right: Complex[ComponentType2]): Complex[subOperator.Result] =
      Complex[subOperator.Result](left.real - right.real, left.imaginary - right.imaginary)
  
  given multiply
      [ComponentType, ComponentType2]
      (using multiply: MulOperator[ComponentType, ComponentType2])
      (using addOperator: AddOperator[multiply.Result, multiply.Result])
      (using subOperator: SubOperator[multiply.Result, multiply.Result])
      : MulOperator[Complex[ComponentType], Complex[ComponentType2]] with
    type Result = Complex[addOperator.Result | subOperator.Result]

    def mul
        (left: Complex[ComponentType], right: Complex[ComponentType2])
        : Complex[addOperator.Result | subOperator.Result] =
      val ac: multiply.Result = left.real*right.real
      val bd: multiply.Result = left.imaginary*right.imaginary
      val ad: multiply.Result = left.real*right.imaginary
      val bc: multiply.Result = left.imaginary*right.real
    
      Complex(ac - bd, ad + bc)

case class Complex[+ComponentType](real: ComponentType, imaginary: ComponentType)
