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
package quantitative

import language.experimental.captureChecking
import language.experimental.into

import anticipation.*
import gossamer.*
import proscenium.*
import symbolism.*

extension [units <: Measure](quantity: Quantity[units])
  transparent inline def in[units2[power <: Nat] <: Units[power, ?]]: Any =
    ${Quantitative.norm[units, units2]('quantity)}

extension [units <: Measure](quantity: into Quantity[units])
  @targetName("plus")
  transparent inline infix def + [units2 <: Measure](quantity2: Quantity[units2]): Any =
    ${Quantitative.add[units, units2]('quantity, 'quantity2, '{false})}

  @targetName("minus")
  transparent inline infix def - [units2 <: Measure](quantity2: Quantity[units2]): Any =
    ${Quantitative.add[units, units2]('quantity, 'quantity2, '{true})}

  transparent inline def invert: Any = Quantity[Measure](1.0)/quantity

  @targetName("times2")
  transparent inline infix def * [units2 <: Measure](inline quantity2: Quantity[units2]): Any =
    ${Quantitative.multiply[units, units2]('quantity, 'quantity2, false)}

  @targetName("times3")
  transparent inline infix def * [units2 <: Measure](inline double: into Double): Any =
    quantity*Quantity(double)

  @targetName("divide2")
  transparent inline infix def / [units2 <: Measure](inline quantity2: Quantity[units2]): Any =

    ${Quantitative.multiply[units, units2]('quantity, 'quantity2, true)}

  @targetName("divide3")
  transparent inline infix def / [units2 <: Measure](inline double: into Double): Any =
    quantity/Quantity(double)

  inline def sqrt(using root: Quantity[units] is Rootable[2]): root.Result =
    root.root(quantity)

  inline def cbrt(using root: Quantity[units] is Rootable[3]): root.Result =
    root.root(quantity)

  inline def units: Map[Text, Int] = ${Quantitative.collectUnits[units]}
  inline def express(using Decimalizer): Text = t"${quantity.value} ${Quantity.expressUnits(units)}"
  inline def dimension: Text = ${Quantitative.describe[units]}
