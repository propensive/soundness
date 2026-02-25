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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import geodesy.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import proscenium.*
import quantitative.*
import spectacular.*
import symbolism.*

import scala.annotation.*

object Complex:
  inline given showable: [part: {Showable, Zeroic, Commensurable against part, Negatable to part}]
  =>  Complex[part] is Showable =

    complex =>
      compiletime.summonFrom:
        case distributive: (`part` is Distributive) =>
          provide[Complex[distributive.Operand] is Showable]:
            provide[distributive.Operand is Zeroic]:
              val reParts: List[distributive.Operand] = distributive.parts(complex.real)
              val imParts: List[distributive.Operand] = distributive.parts(complex.imaginary)
              val parts = reParts.zip(imParts).map(Complex(_, _).show)
              val parts2 = parts.zip(imParts).map: (part, im) =>
                if im == zero[distributive.Operand] then part else t"($part)"

              distributive.place(complex.real, parts2)

        case _ =>
          if complex.imaginary == zero[part] then complex.real.show
          else if complex.real == zero[part] then t"${complex.imaginary.show}ℐ"
          else if complex.imaginary < zero[part]
          then t"${complex.real.show} - ${(-complex.imaginary).show}ℐ"
          else t"${complex.real.show} + ${complex.imaginary.show}ℐ"


  given addable: [result, component2, component: Addable by component2 to result as addable]
  =>  Complex[component] is Addable by Complex[component2] to Complex[result] =

    Addable[Complex[component], Complex[component2], Complex[result]]:
      (left, right) =>
        Complex[result](left.real + right.real, left.imaginary + right.imaginary)


  given subtractable
  :   [ result,
        component2,
        component: Subtractable by component2 to result as subtractable ]
  =>  Complex[component] is Subtractable by Complex[component2] to Complex[result] =

    Subtractable[Complex[component], Complex[component2], Complex[result]]:
      (left, right) =>
        Complex[result](left.real - right.real, left.imaginary - right.imaginary)


  inline given multiplicable: [component, component2]
  =>  ( multiplication: component is Multiplicable by component2,
        addition:       multiplication.Result is Addable by multiplication.Result,
        subtraction:    multiplication.Result is Subtractable by multiplication.Result )
  =>  Complex[component] is Multiplicable by Complex[component2] =

    Multiplicable
      [ Complex[component], Complex[component2], Complex[addition.Result | subtraction.Result] ]:

      (left, right) =>
        Complex
          ( left.real*right.real - left.imaginary*right.imaginary,
            left.real*right.imaginary + left.imaginary*right.real )


  given divisible
  :   [ component, dividend <: Complex[component], component2, divisor <: Complex[component2] ]
  =>  ( multiplication:  component is Multiplicable by component2,
        multiplication2: component2 is Multiplicable by component2,
        negatable:       component is Negatable to component,
        addition:        multiplication.Result is Addable by multiplication.Result,
        addition2:       multiplication2.Result is Addable by multiplication2.Result,
        divisible:       addition.Result is Divisible by addition2.Result )
  =>  dividend is Divisible:

        type Self = dividend
        type Operand = divisor
        type Result = Complex[divisible.Result]

        def divide(left: dividend, right: divisor): Complex[divisible.Result] =
          val denominator: addition2.Result =
            right.real*right.real + right.imaginary*right.imaginary

          Complex
            ( (left.real*right.real + left.imaginary*right.imaginary)/denominator,
              (left.imaginary*right.real + (-left.real)*right.imaginary)/denominator )

  given negatable: [component: Negatable]
  =>  Complex[component] is Negatable to Complex[component.Result] = complex =>

      Complex(-complex.real, -complex.imaginary)


  def apply[component: Multiplicable by Double as multiplication]
    ( modulus: component, argument: Angle )
  :   Complex[multiplication.Result] =

    Complex(modulus*math.cos(argument.radians), modulus*math.sin(argument.radians))


case class Complex[component](real: component, imaginary: component):
  inline def argument
    ( using multiplication: component is Multiplicable by component,
            addition:       multiplication.Result is Addable by multiplication.Result,
            sqrt:           addition.Result is Rootable[2],
            division:       component is Divisible by sqrt.Result,
            equality:       division.Result =:= Double )
  :   Angle =

    Angle(scala.math.atan2(imaginary/modulus, real/modulus))


  inline def modulus
    ( using multiplication: component is Multiplicable by component,
            addition:       multiplication.Result is Addable by multiplication.Result,
            squareRoot:     addition.Result is Rootable[2] )
  :   squareRoot.Result =

    squareRoot.root(real*real + imaginary*imaginary)


  inline def sqrt
    ( using multiplication:  component is Multiplicable by component,
            addition:        multiplication.Result is Addable by multiplication.Result,
            sqrt:            addition.Result is Rootable[2],
            division:        component is Divisible by sqrt.Result,
            equality:        division.Result =:= Double,
            sqrt2:           sqrt.Result is Rootable[2],
            multiplication2: sqrt2.Result is Multiplicable by Double )
  :   Complex[multiplication2.Result] =

    Complex(modulus.sqrt, argument/2.0)


  @targetName("conjugate")
  inline def unary_~(using neg: component is Negatable): Complex[component | neg.Result] =
    Complex(real, -imaginary)
