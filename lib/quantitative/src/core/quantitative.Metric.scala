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
┃    Soundness, version 0.49.0.                                                                    ┃
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

import hypotenuse.*
import symbolism.*

class Metric(val name: String, val symbol: String, val exponent: Int, val base: 2 | 10):
  def apply[units <: Measure](unit: MetricUnit[units]): Quantity[units] =
    Quantity(unit*Quantity(base**exponent))

object NoPrefix extends Metric("", "", 0, 10)

object Deka extends Metric("deka", "da", 1, 10)
object Hecto extends Metric("hecto", "h", 2, 10)
object Kilo extends Metric("kilo", "k", 3, 10)
object Mega extends Metric("mega", "M", 6, 10)
object Giga extends Metric("giga", "G", 9, 10)
object Tera extends Metric("tera", "T", 12, 10)
object Peta extends Metric("peta", "P", 15, 10)
object Exa extends Metric("exa", "E", 18, 10)
object Zetta extends Metric("zetta", "Z", 21, 10)
object Yotta extends Metric("yotta", "Y", 24, 10)
object Ronna extends Metric("ronna", "R", 27, 10)
object Quetta extends Metric("quetta", "Q", 30, 10)

object Deci extends Metric("deci", "d", -1, 10)
object Centi extends Metric("centi", "c", -2, 10)
object Milli extends Metric("milli", "m", -3, 10)
object Micro extends Metric("micro", "µ", -6, 10)
object Nano extends Metric("nano", "n", -9, 10)
object Pico extends Metric("pico", "p", -12, 10)
object Femto extends Metric("femto", "f", -15, 10)
object Atto extends Metric("atto", "a", -18, 10)
object Zepto extends Metric("zepto", "z", -21, 10)
object Yocto extends Metric("yocto", "y", -24, 10)
object Ronto extends Metric("ronto", "r", -27, 10)
object Quecto extends Metric("quecto", "q", -30, 10)

object Kibi extends Metric("kibi", "Ki", 10, 2)
object Mebi extends Metric("mebi", "Mi", 20, 2)
object Gibi extends Metric("gibi", "Gi", 30, 2)
object Tebi extends Metric("tebi", "Ti", 40, 2)
object Pebi extends Metric("pebi", "Pi", 50, 2)
object Exbi extends Metric("exbi", "Ei", 60, 2)
object Zebi extends Metric("zebi", "Zi", 70, 2)
object Yobi extends Metric("yobi", "Yi", 80, 2)
