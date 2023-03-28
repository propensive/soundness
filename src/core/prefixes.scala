/*
    Quantify, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quantify

class SiPrefix(name: String, symbol: String, exponent: Int, base: 2 | 10):
  def apply[UnitsType <: Units[?, ?]](unit: SiUnit[UnitsType]): Quantity[UnitsType] =
    unit*math.pow(base, exponent)

object Deka extends SiPrefix("deka", "da", 1, 10)
object Hecto extends SiPrefix("hecto", "h", 2, 10)
object Kilo extends SiPrefix("kilo", "k", 3, 10)
object Mega extends SiPrefix("mega", "M", 6, 10)
object Giga extends SiPrefix("giga", "G", 9, 10)
object Tera extends SiPrefix("tera", "T", 12, 10)
object Peta extends SiPrefix("peta", "P", 15, 10)
object Exa extends SiPrefix("exa", "E", 18, 10)
object Zetta extends SiPrefix("zetta", "Z", 21, 10)
object Yotta extends SiPrefix("yotta", "Y", 24, 10)
object Ronna extends SiPrefix("ronna", "R", 27, 10)
object Quetta extends SiPrefix("quetta", "Q", 30, 10)

object Deci extends SiPrefix("kilo", "d", -1, 10)
object Centi extends SiPrefix("hecto", "c", -2, 10)
object Milli extends SiPrefix("kilo", "m", -3, 10)
object Micro extends SiPrefix("mega", "µ", -6, 10)
object Nano extends SiPrefix("giga", "n", -9, 10)
object Pico extends SiPrefix("tera", "p", -12, 10)
object Femto extends SiPrefix("peta", "f", -15, 10)
object Atto extends SiPrefix("exa", "a", -18, 10)
object Zepto extends SiPrefix("zetta", "z", -21, 10)
object Yocto extends SiPrefix("yotta", "y", -24, 10)
object Ronto extends SiPrefix("ronna", "r", -27, 10)
object Quecto extends SiPrefix("quetta", "q", -30, 10)

object Kibi extends SiPrefix("kibi", "Ki", 10, 2)
object Mebi extends SiPrefix("mebi", "Mi", 20, 2)
object Gibi extends SiPrefix("gibi", "Gi", 30, 2)
object Tebi extends SiPrefix("tebi", "Ti", 40, 2)
object Pebi extends SiPrefix("pebi", "Pi", 50, 2)
object Exbi extends SiPrefix("exbi", "Ei", 60, 2)
object Zebi extends SiPrefix("zebi", "Zi", 70, 2)
object Yobi extends SiPrefix("yobi", "Yi", 80, 2)