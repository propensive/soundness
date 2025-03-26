                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
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
  given show: [component: Showable] => Complex[component] is Showable = complex =>
   t"${complex.real.show} + ${complex.imaginary.show}𝒾"

  inline given quantity: [units <: Measure] => Decimalizer => Complex[Quantity[units]] is Showable:
    def text(value: Complex[Quantity[units]]): Text =
      t"${value.real.value} + ${value.imaginary.value}𝒾 ${Quantity.expressUnits(value.real.units)}"

  inline given addable: [component: Addable by component as addable]
               =>  Complex[component] is Addable by Complex[component] =
    Addable[Complex[component], Complex[component], Complex[addable.Result]]:
      (left, right) =>
        Complex[addable.Result](left.real + right.real, left.imaginary + right.imaginary)

  inline given subtractable: [component: Subtractable by component as subtractable]
               =>  Complex[component] is Subtractable by Complex[component] =
    Subtractable[Complex[component], Complex[component], Complex[subtractable.Result]]:
      (left, right) =>
        Complex[subtractable.Result](left.real - right.real, left.imaginary - right.imaginary)

  inline given multiplicable: [component]
               => (multiplication: component is Multiplicable by component,
                   addition:       multiplication.Result is Addable by multiplication.Result,
                   subtraction:    multiplication.Result is Subtractable by multiplication.Result)
               =>  Complex[component] is Multiplicable by Complex[component] =
    Multiplicable[Complex[component],
                  Complex[component],
                  Complex[addition.Result | subtraction.Result]]:
      (left, right) =>
        Complex
         (left.real*right.real - left.imaginary*right.imaginary,
          left.real*right.imaginary + left.imaginary*right.real)

  def polar[component: Multiplicable by Double as multiplication]
       (modulus: component, argument: Double)
  :     Complex[multiplication.Result] =
    Complex(modulus*math.cos(argument), modulus*math.sin(argument))

case class Complex[component](real: component, imaginary: component):
  @targetName("add")
  inline infix def + [component2](right: Complex[component2])
                     (using addition: component is Addable by component2)
  :     Complex[addition.Result] =
    Complex(this.real + right.real, this.imaginary + right.imaginary)

  @targetName("sub")
  inline infix def - [component2](right: Complex[component2])
                     (using subtraction: component is Subtractable by component2)
  :     Complex[subtraction.Result] =
    Complex(this.real - right.real, this.imaginary - right.imaginary)

  @targetName("mul")
  inline infix def * [component2](right: Complex[component2])
                     (using multiplication: component is Multiplicable by component2,
                            addition:       multiplication.Result is Addable by
                                             multiplication.Result,
                            subtraction:    multiplication.Result is Subtractable by
                                             multiplication.Result)
  :     Complex[subtraction.Result | addition.Result] =

    Complex
     (real*right.real - imaginary*right.imaginary, real*right.imaginary + imaginary*right.real)

  @targetName("div")
  inline infix def / [component2](right: Complex[component2])
                     (using multiplication:  component is Multiplicable by component2,
                            multiplication2: component2 is Multiplicable by component2,
                            addition:        multiplication.Result is Addable by
                                              multiplication.Result,
                            addition2:       multiplication2.Result is Addable by
                                              multiplication2.Result,
                            subtraction:     multiplication.Result is Subtractable by
                                              multiplication.Result,
                            divisible:       subtraction.Result
                                             | addition.Result is Divisible by addition2.Result)
  :     Complex[divisible.Result] =

    val divisor = right.real*right.real + right.imaginary*right.imaginary

    Complex
     ((real*right.real + imaginary*right.imaginary)/divisor,
      (imaginary*right.real - real*right.imaginary)/divisor)

  inline def argument
              (using multiplication: component is Multiplicable by component,
               addition:      multiplication.Result is Addable by multiplication.Result,
               sqrt:          addition.Result is Rootable[2],
               division:      component is Divisible by sqrt.Result,
               equality:      division.Result =:= Double)
  :     Double =

    scala.math.atan2(imaginary/modulus, real/modulus)

  inline def modulus
              (using multiplication: component is Multiplicable by component,
                     addition:       multiplication.Result is Addable by multiplication.Result,
                     squareRoot:     addition.Result is Rootable[2])
  :     squareRoot.Result =
    squareRoot.root(real*real + imaginary*imaginary)

  inline def sqrt
              (using multiplication:  component is Multiplicable by component,
                     addition:        multiplication.Result is Addable by multiplication.Result,
                     sqrt:            addition.Result is Rootable[2],
                     division:        component is Divisible by sqrt.Result,
                     equality:        division.Result =:= Double,
                     sqrt2:           sqrt.Result is Rootable[2],
                     multiplication2: sqrt2.Result is Multiplicable by Double)
  :     Complex[multiplication2.Result] =
    Complex.polar(modulus.sqrt, argument/2.0)

  @targetName("conjugate")
  inline def unary_~(using neg: component is Negatable): Complex[component | neg.Result] =
    Complex(real, -imaginary)

  @targetName("neg")
  inline def unary_-(using neg: component is Negatable): Complex[component | neg.Result] =
    Complex(-real, -imaginary)
