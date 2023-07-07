package baroque

import gossamer.*
import rudiments.*
import spectacular.*
import quantitative.*
import anticipation.*

val I: Complex[Double] = Complex(0.0, 1.0)

object Complex:
  given show[ComponentType: Show]: Show[Complex[ComponentType]] = complex =>
   t"${complex.real.show} + ${complex.imaginary.show}𝒊"
  
  inline given quantityShow
      [UnitsType <: Measure]
      (using Decimalizer)
      : Show[Complex[Quantity[UnitsType]]] =
    
    new Show[Complex[Quantity[UnitsType]]]:
      def apply(value: Complex[Quantity[UnitsType]]): Text =
        t"${value.real.value} + ${value.imaginary.value}𝒊 ${Quantity.renderUnits(value.real.units)}"
  
case class Complex[+ComponentType](real: ComponentType, imaginary: ComponentType):
  def +
      [ComponentType2]
      (right: Complex[ComponentType2])
      (using add: Add[ComponentType, ComponentType2])
      : Complex[add.Result] =
    Complex[add.Result](add(real, right.real, false), add(imaginary, right.imaginary, false))
  
  def -
      [ComponentType2]
      (right: Complex[ComponentType2])
      (using add: Add[ComponentType, ComponentType2])
      : Complex[add.Result] =
    Complex[add.Result](add(real, right.real, true), add(imaginary, right.imaginary, true))
  
  def *
      [ComponentType2]
      (right: Complex[ComponentType2])
      (using multiply: Multiply[ComponentType, ComponentType2])
      (using add: Add[multiply.Result, multiply.Result])
      : Complex[add.Result] =
    val ac: multiply.Result = multiply(real, right.real)
    val bd: multiply.Result = multiply(imaginary, right.imaginary)
    val ad: multiply.Result = multiply(real, right.imaginary)
    val bc: multiply.Result = multiply(imaginary, right.real)
    
    Complex[add.Result](add(ac, bd, true), add(ad, bc, false))