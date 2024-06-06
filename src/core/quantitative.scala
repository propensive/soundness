package quantitative

import language.experimental.captureChecking
import language.experimental.into

import anticipation.*
import gossamer.*
import rudiments.*
import symbolism.*

extension [UnitsType <: Measure](inline quantity: Quantity[UnitsType])
  @targetName("plus")
  transparent inline infix def + [UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${Quantitative.add[UnitsType, UnitsType2]('quantity, 'quantity2, '{false})}

  @targetName("minus")
  transparent inline infix def - [UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${Quantitative.add[UnitsType, UnitsType2]('quantity, 'quantity2, '{true})}

  transparent inline def invert: Any = Quantity[Measure](1.0)/quantity

  transparent inline def in[UnitsType2[power <: Nat] <: Units[power, ?]]: Any =
    ${Quantitative.norm[UnitsType, UnitsType2]('quantity)}

  @targetName("times2")
  transparent inline infix def * [UnitsType2 <: Measure](inline quantity2: Quantity[UnitsType2])
          : Any =

    ${Quantitative.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, false)}

  @targetName("times3")
  transparent inline infix def * [UnitsType2 <: Measure](inline double: into Double): Any =
    quantity*Quantity(double)

  @targetName("divide2")
  transparent inline infix def / [UnitsType2 <: Measure](inline quantity2: Quantity[UnitsType2])
          : Any =

    ${Quantitative.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  @targetName("divide3")
  transparent inline infix def / [UnitsType2 <: Measure](inline double: into Double): Any =
    quantity/Quantity(double)

  inline def sqrt(using root: RootOperator[2, Quantity[UnitsType]]): root.Result =
    root.root(quantity)

  inline def cbrt(using root: RootOperator[3, Quantity[UnitsType]]): root.Result =
    root.root(quantity)

  inline def units: Map[Text, Int] = ${Quantitative.collectUnits[UnitsType]}
  inline def render(using Decimalizer): Text = t"${quantity.value} ${Quantity.renderUnits(units)}"
  inline def dimension: Text = ${Quantitative.describe[UnitsType]}

extension (value: Double)
  @targetName("times")
  infix def * [UnitsType <: Measure](quantity: Quantity[UnitsType]): Quantity[UnitsType] =
    Quantity(quantity*value)

  @targetName("divide")
  transparent inline infix def / [UnitsType <: Measure](quantity: Quantity[UnitsType]): Any =
    ((1.0/value)*quantity).invert
