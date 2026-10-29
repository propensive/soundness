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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.annotation.*
import scala.compiletime.*
import scala.{compiletime, math}

import anticipation.*
import geodesy.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*

object Complex:
  // The two representations: two primitive `double`s for every component that is a `Double`
  // underneath — `Double` itself, a quantity, an `F64`; the component is a phantom there — and a
  // pair of references for any other. Which one a value has is decided by the component's
  // runtime class, so generic code and double-specific code always agree on it.
  final class Doubles[component](val real: Double, val imaginary: Double)
  extends Complex[component]:
    override def equals(right: Any): Boolean = right.asMatchable match
      case that: Doubles[?] => real == that.real && imaginary == that.imaginary
      case _                => false

    override def hashCode: Int =
      31*java.lang.Double.hashCode(real) + java.lang.Double.hashCode(imaginary)

    override def toString: String = s"Complex($real, $imaginary)"

  final class Boxed[component](val real: component, val imaginary: component)
  extends Complex[component]:
    override def equals(right: Any): Boolean = right.asMatchable match
      case that: Boxed[?] => real == that.real && imaginary == that.imaginary
      case _              => false

    override def hashCode: Int = 31*real.## + imaginary.##
    override def toString: String = s"Complex($real, $imaginary)"

  // The component arithmetic, dispatched as symbolism's operators are: an `AddOp` first (the
  // macro-generated, unit-checked operations of quantities), else the `Addable` summoned with
  // its result open, each bound at its own type so a primitive's `inline def` is one
  // instruction. The casts are symbolism's own; the backticks keep the type parameters.
  private inline def plus[a, b, r](left: a, right: b): r = summonFrom:
    case op: (`a` is AddOp by `b`) =>
      op.op(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

    case addable: (`a` is Addable by `b`) =>
      addable.add(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

  private inline def minus[a, b, r](left: a, right: b): r = summonFrom:
    case op: (`a` is SubOp by `b`) =>
      op.op(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

    case subtractable: (`a` is Subtractable by `b`) =>
      subtractable.subtract(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

  private inline def times[a, b, r](left: a, right: b): r = summonFrom:
    case op: (`a` is MulOp by `b`) =>
      op.op(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

    case multiplicable: (`a` is Multiplicable by `b`) =>
      multiplicable.multiply(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

  private inline def over[a, b, r](left: a, right: b): r = summonFrom:
    case op: (`a` is DivOp by `b`) =>
      op.op(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

    case divisible: (`a` is Divisible by `b`) =>
      divisible.divide(left.asInstanceOf, right.asInstanceOf).asInstanceOf[r]

  // The representation is chosen at compile time where the component is known to be a
  // `Double` underneath (by `Underlying` evidence, or by being `Double`), and by one type test
  // on the value where the code is generic.
  inline def apply[component](real: component, imaginary: component): Complex[component] =
    summonFrom:
      case _: Underlying[`component`, Double] =>
        Doubles[component](real.asInstanceOf[Double], imaginary.asInstanceOf[Double])

      case _: (`component` =:= Double) =>
        Doubles[component](real.asInstanceOf[Double], imaginary.asInstanceOf[Double])

      case _ => (real: Any).asMatchable match
        case double: Double => Doubles[component](double, imaginary.asInstanceOf[Double])
        case _              => Boxed[component](real, imaginary)

  def unapply[component](complex: Complex[component]): Some[(component, component)] =
    Some((complex.real, complex.imaginary))

  extension [component](complex: Complex[component])
    inline def real: component = summonFrom:
      case _: Underlying[`component`, Double] =>
        complex.asInstanceOf[Doubles[component]].real.asInstanceOf[component]

      case _: (`component` =:= Double) =>
        complex.asInstanceOf[Doubles[component]].real.asInstanceOf[component]

      case _ => complex match
        case doubles: Doubles[component] @scala.unchecked => doubles.real.asInstanceOf[component]
        case boxed: Boxed[component] @scala.unchecked     => boxed.real

    inline def imaginary: component = summonFrom:
      case _: Underlying[`component`, Double] =>
        complex.asInstanceOf[Doubles[component]].imaginary.asInstanceOf[component]

      case _: (`component` =:= Double) =>
        complex.asInstanceOf[Doubles[component]].imaginary.asInstanceOf[component]

      case _ => complex match
        case doubles: Doubles[component] @scala.unchecked =>
          doubles.imaginary.asInstanceOf[component]

        case boxed: Boxed[component] @scala.unchecked => boxed.imaginary

    inline def argument
      ( using multiplication: component is Multiplicable by component,
              addition:       multiplication.Result is Addable by multiplication.Result,
              sqrt:           addition.Result is Rootable[2],
              division:       component is Divisible by sqrt.Result,
              equality:       division.Result =:= Double )
    :   Angle =

      Angle(scala.math.atan2(complex.imaginary/complex.modulus, complex.real/complex.modulus))

    inline def modulus
      ( using multiplication: component is Multiplicable by component,
              addition:       multiplication.Result is Addable by multiplication.Result,
              squareRoot:     addition.Result is Rootable[2] )
    :   squareRoot.Result =

      val real = complex.real
      val imaginary = complex.imaginary

      squareRoot.root
        ( plus[multiplication.Result, multiplication.Result, addition.Result]
            ( times[component, component, multiplication.Result](real, real),
              times[component, component, multiplication.Result](imaginary, imaginary) ) )

    inline def sqrt
      ( using multiplication:  component is Multiplicable by component,
              addition:        multiplication.Result is Addable by multiplication.Result,
              sqrt:            addition.Result is Rootable[2],
              division:        component is Divisible by sqrt.Result,
              equality:        division.Result =:= Double,
              sqrt2:           sqrt.Result is Rootable[2],
              multiplication2: sqrt2.Result is Multiplicable by Double )
    :   Complex[multiplication2.Result] =

      // `sqrt2.root`, not `.sqrt`: this extension's own name shadows hypotenuse's here.
      Complex(sqrt2.root(complex.modulus), complex.argument/2.0)

    @targetName("conjugate")
    inline def unary_~(using neg: component is Negatable): Complex[component | neg.Result] =
      Complex[component | neg.Result](complex.real, -complex.imaginary)

  inline given showable: [part: {Showable, Zeroic, Commensurable against part, Negatable to part}]
  =>  Complex[part] is Showable =

    complex =>
      compiletime.summonFrom:
        case distributive: (`part` is Distributive) =>
          showDistributive[part](complex, distributive)
            ( using infer[Complex[distributive.Operand] is Showable],
                    infer[distributive.Operand is Zeroic] )

        case _ =>
          if complex.imaginary == zero[part] then complex.real.show
          else if complex.real == zero[part] then t"${complex.imaginary}ℐ"
          else if complex.imaginary < zero[part]
          then t"${complex.real} - ${(-complex.imaginary)}ℐ"
          else t"${complex.real} + ${complex.imaginary}ℐ"

  private inline def showDistributive[part]
    ( complex: Complex[part], distributive: part is Distributive )
    ( using Complex[distributive.Operand] is Showable, distributive.Operand is Zeroic )
  :   anticipation.Text =

    val reParts: List[distributive.Operand] = distributive.parts(complex.real)
    val imParts: List[distributive.Operand] = distributive.parts(complex.imaginary)
    val parts = reParts.zip(imParts).map(Complex(_, _).show)

    val parts2 = parts.zip(imParts).map: (part, im) =>
      if im == zero[distributive.Operand] then part else t"($part)"

    distributive.place(complex.real, parts2)

  given inspectable: [component: Inspectable, complex <: Complex[component]]
  =>  complex is Inspectable =

    complex =>
      val imaginary = complex.imaginary.inspect.s

      val body =
        if imaginary.startsWith("-") then "-"+imaginary.substring(1).nn else "+"+imaginary

      (complex.real.inspect.s+body+"i").tt

  // The arithmetic instances are inline, resolving the component's operations at the call
  // site (see `plus`), so the only cost of `c + d` on doubles is the result's allocation.
  // The type members are in each given's type (the head's refinement), not its body: a member
  // declared in the body is a projection from the given's instance, which an inline given mints
  // afresh at every use, so two uses' `Result`s would never unify.
  inline given addable: [result, component2, component: Addable by component2 to result]
  =>  ( Addable { type Self = Complex[component]; type Operand = Complex[component2]
          type Result = Complex[result] } ):

    inline def add(left: Complex[component], right: Complex[component2]): Complex[result] =
      Complex[result]
        ( plus[component, component2, result](left.real, right.real),
          plus[component, component2, result](left.imaginary, right.imaginary) )

  inline given subtractable
  :   [ result, component2, component: Subtractable by component2 to result ]
  =>  ( Subtractable { type Self = Complex[component]; type Operand = Complex[component2]
          type Result = Complex[result] } ):

    inline def subtract(left: Complex[component], right: Complex[component2]): Complex[result] =
      Complex[result]
        ( minus[component, component2, result](left.real, right.real),
          minus[component, component2, result](left.imaginary, right.imaginary) )

  // The result types are type parameters, inferred through the `using` clause, rather than
  // the parameters' `Result` members: a member named by a parameter's path is invisible in the
  // given's class type, so a use's `Result` would be an unknown projection.
  inline given multiplicable: [component, component2, product, sum, difference]
  =>  ( multiplication: component is Multiplicable by component2 to product,
        addition:       product is Addable by product to sum,
        subtraction:    product is Subtractable by product to difference )
  =>  ( Multiplicable { type Self = Complex[component]; type Operand = Complex[component2]
          type Result = Complex[sum | difference] } ):

    type Product = product
    type Sum = sum
    type Difference = difference

    inline def multiply(left: Complex[component], right: Complex[component2])
    :   Complex[Sum | Difference] =

      val leftReal = left.real
      val leftImaginary = left.imaginary
      val rightReal = right.real
      val rightImaginary = right.imaginary

      Complex[Sum | Difference]
        ( minus[Product, Product, Difference]
            ( times[component, component2, Product](leftReal, rightReal),
              times[component, component2, Product](leftImaginary, rightImaginary) ),
          plus[Product, Product, Sum]
            ( times[component, component2, Product](leftReal, rightImaginary),
              times[component, component2, Product](leftImaginary, rightReal) ) )

  // Scaling by a real number, on either side, as in `i*1.8` or `2.0*i`.
  inline given multiplicableByDouble: [component, scaled]
  =>  ( multiplication: component is Multiplicable by Double to scaled )
  =>  ( Multiplicable { type Self = Complex[component]; type Operand = Double
          type Result = Complex[scaled] } ):

    type Scaled = scaled

    inline def multiply(complex: Complex[component], scalar: Double): Complex[Scaled] =
      Complex[Scaled]
        ( times[component, Double, Scaled](complex.real, scalar),
          times[component, Double, Scaled](complex.imaginary, scalar) )

  inline given doubleMultiplicable: [component, scaled]
  =>  ( multiplication: Double is Multiplicable by component to scaled )
  =>  ( Multiplicable { type Self = Double; type Operand = Complex[component]
          type Result = Complex[scaled] } ):

    type Scaled = scaled

    inline def multiply(scalar: Double, complex: Complex[component]): Complex[Scaled] =
      Complex[Scaled]
        ( times[Double, component, Scaled](scalar, complex.real),
          times[Double, component, Scaled](scalar, complex.imaginary) )

  inline given divisible
  :   [ component, dividend <: Complex[component], component2, divisor <: Complex[component2],
        product, product2, sum, sum2, quotient ]
  =>  ( multiplication:  component is Multiplicable by component2 to product,
        multiplication2: component2 is Multiplicable by component2 to product2,
        negatable:       component is Negatable to component,
        addition:        product is Addable by product to sum,
        addition2:       product2 is Addable by product2 to sum2,
        divisible:       sum is Divisible by sum2 to quotient )
  =>  ( Divisible { type Self = dividend; type Operand = divisor
          type Result = Complex[quotient] } ):

    type Product = product
    type Product2 = product2
    type Sum = sum
    type Sum2 = sum2
    type Quotient = quotient

    inline def divide(left: dividend, right: divisor): Complex[Quotient] =
      val leftReal = left.real
      val leftImaginary = left.imaginary
      val rightReal = right.real
      val rightImaginary = right.imaginary

      val denominator: Sum2 =
        plus[Product2, Product2, Sum2]
          ( times[component2, component2, Product2](rightReal, rightReal),
            times[component2, component2, Product2](rightImaginary, rightImaginary) )

      val negatedReal: component =
        summonInline[component is Negatable to component].negate(leftReal)

      Complex[Quotient]
        ( over[Sum, Sum2, Quotient]
            ( plus[Product, Product, Sum]
                ( times[component, component2, Product](leftReal, rightReal),
                  times[component, component2, Product](leftImaginary, rightImaginary) ),
              denominator ),
          over[Sum, Sum2, Quotient]
            ( plus[Product, Product, Sum]
                ( times[component, component2, Product](leftImaginary, rightReal),
                  times[component, component2, Product](negatedReal, rightImaginary) ),
              denominator ) )

  inline given negatable: [component, negated]
  =>  ( negation: component is Negatable to negated )
  =>  ( Negatable { type Self = Complex[component]; type Result = Complex[negated] } ):

    type Negated = negated

    inline def negate(complex: Complex[component]): Complex[Negated] =
      val negation = summonInline[component is Negatable]

      Complex[Negated]
        ( negation.negate(complex.real).asInstanceOf[Negated],
          negation.negate(complex.imaginary).asInstanceOf[Negated] )

  inline def apply[component: Multiplicable by Double as multiplication]
    ( modulus: component, argument: Angle )
  :   Complex[multiplication.Result] =

    Complex[multiplication.Result]
      ( times[component, Double, multiplication.Result](modulus, math.cos(argument.radians)),
        times[component, Double, multiplication.Result](modulus, math.sin(argument.radians)) )

// A complex number over any component type. Its two representations are the companion's
// `Doubles` and `Boxed`; the constructor, the accessors and the arithmetic instances are
// inline, and the arithmetic resolves the component's operations at the call site (`plus` and
// its siblings), so `c*d` on two `Complex[Double]`s is four `dmul`s, a `dsub`, a `dadd` and one
// allocation of two unboxed fields — where the case class it replaced allocated an object and
// two boxed doubles, and unboxed them on every read.
sealed abstract class Complex[component]
