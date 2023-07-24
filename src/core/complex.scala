package baroque

import gossamer.*
import rudiments.*
import spectacular.*
import quantitative.*
import anticipation.*
import symbolism.*

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
      (using add: Operator["+", ComponentType, ComponentType2])
      : Complex[add.Result] =
    Complex[add.Result](add(real, right.real), add(imaginary, right.imaginary))
  
  def -
      [ComponentType2]
      (right: Complex[ComponentType2])
      (using subtract: Operator["-", ComponentType, ComponentType2])
      : Complex[subtract.Result] =
    Complex[subtract.Result](subtract(real, right.real), subtract(imaginary, right.imaginary))
  
  def *
      [ComponentType2]
      (right: Complex[ComponentType2])
      (using multiply: Operator["*", ComponentType, ComponentType2])
      (using add: Operator["+", multiply.Result, multiply.Result])
      (using subtract: Operator["-", multiply.Result, multiply.Result])
      : Complex[add.Result | subtract.Result] =
    val ac: multiply.Result = multiply(real, right.real)
    val bd: multiply.Result = multiply(imaginary, right.imaginary)
    val ad: multiply.Result = multiply(real, right.imaginary)
    val bc: multiply.Result = multiply(imaginary, right.real)
    
    Complex(subtract(ac, bd), add(ad, bc))
