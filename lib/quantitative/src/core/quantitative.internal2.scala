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
package quantitative

import scala.{compiletime, math}

import anticipation.*
import gossamer.*
import prepositional.*
import symbolism.*

object internal2:
  trait Protoquantity:
    extension [units <: Measure](quantity: Quantity[units])
      transparent inline def to[units2[power <: Nat] <: Units[power, ?]]: Any =
        ${quantitative.internal.norm[units, units2]('quantity)}

      transparent inline def invert: Any = Quantity[Measure](1.0)/quantity


      inline def normalize[units2 <: Measure](using normalizable: units is Normalizable to units2)
      :   Quantity[units2] =

        normalizable.normalize(quantity)


      inline def sqrt(using root: Quantity[units] is Rootable[2]): root.Result = root.root(quantity)
      inline def cbrt(using root: Quantity[units] is Rootable[3]): root.Result = root.root(quantity)
      inline def units: Map[Text, Int] = ${quantitative.internal.collectUnits[units]}

      inline def express(using Decimalizer): Text = compiletime.summonFrom:
        case prefixes: (Prefixes `on` `units`) =>
          val prefix = prefixes.select(quantity.value)
          val scaled = quantity.value/math.pow(prefix.base.toDouble, prefix.exponent.toDouble)
          t"$scaled ${prefix.symbol.tt}${Quantity.expressUnits(units)}"

        case prefixes: Prefixes =>
          val prefix = prefixes.select(quantity.value)
          val scaled = quantity.value/math.pow(prefix.base.toDouble, prefix.exponent.toDouble)
          t"$scaled ${prefix.symbol.tt}${Quantity.expressUnits(units)}"

        case _ =>
          t"${quantity.value} ${Quantity.expressUnits(units)}"

      inline def dimension: Text = ${quantitative.internal.describe[units]}
