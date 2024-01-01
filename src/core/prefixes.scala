/*
    Quantitative, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quantitative

import language.experimental.captureChecking

class MetricPrefix(val name: String, val symbol: String, val exponent: Int, val base: 2 | 10):
  def apply[UnitsType <: Measure](unit: MetricUnit[UnitsType]): Quantity[UnitsType] =
    Quantity(unit*Quantity(math.pow(base, exponent)))

object NoPrefix extends MetricPrefix("", "", 0, 10)

object Deka extends MetricPrefix("deka", "da", 1, 10)
object Hecto extends MetricPrefix("hecto", "h", 2, 10)
object Kilo extends MetricPrefix("kilo", "k", 3, 10)
object Mega extends MetricPrefix("mega", "M", 6, 10)
object Giga extends MetricPrefix("giga", "G", 9, 10)
object Tera extends MetricPrefix("tera", "T", 12, 10)
object Peta extends MetricPrefix("peta", "P", 15, 10)
object Exa extends MetricPrefix("exa", "E", 18, 10)
object Zetta extends MetricPrefix("zetta", "Z", 21, 10)
object Yotta extends MetricPrefix("yotta", "Y", 24, 10)
object Ronna extends MetricPrefix("ronna", "R", 27, 10)
object Quetta extends MetricPrefix("quetta", "Q", 30, 10)

object Deci extends MetricPrefix("deci", "d", -1, 10)
object Centi extends MetricPrefix("centi", "c", -2, 10)
object Milli extends MetricPrefix("milli", "m", -3, 10)
object Micro extends MetricPrefix("micro", "µ", -6, 10)
object Nano extends MetricPrefix("nano", "n", -9, 10)
object Pico extends MetricPrefix("pico", "p", -12, 10)
object Femto extends MetricPrefix("femto", "f", -15, 10)
object Atto extends MetricPrefix("atto", "a", -18, 10)
object Zepto extends MetricPrefix("zepto", "z", -21, 10)
object Yocto extends MetricPrefix("yocto", "y", -24, 10)
object Ronto extends MetricPrefix("ronto", "r", -27, 10)
object Quecto extends MetricPrefix("quecto", "q", -30, 10)

object Kibi extends MetricPrefix("kibi", "Ki", 10, 2)
object Mebi extends MetricPrefix("mebi", "Mi", 20, 2)
object Gibi extends MetricPrefix("gibi", "Gi", 30, 2)
object Tebi extends MetricPrefix("tebi", "Ti", 40, 2)
object Pebi extends MetricPrefix("pebi", "Pi", 50, 2)
object Exbi extends MetricPrefix("exbi", "Ei", 60, 2)
object Zebi extends MetricPrefix("zebi", "Zi", 70, 2)
object Yobi extends MetricPrefix("yobi", "Yi", 80, 2)
