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

import gossamer.*
import rudiments.*
import anticipation.*
import hypotenuse.*
import symbolism.*
import spectacular.*

import scala.quoted.*

import language.implicitConversions
import language.experimental.captureChecking

trait UnitName[-ValueType]:
  def siPrefix: MetricPrefix = NoPrefix
  def name(): Text
  def text: Text = t"${siPrefix.symbol}${name()}"

object UnitName:
  given UnitName[Metres[1]] = () => t"m"
  given UnitName[Candelas[1]] = () => t"cd"
  given UnitName[Moles[1]] = () => t"mol"
  given UnitName[Amperes[1]] = () => t"A"
  given UnitName[Kelvins[1]] = () => t"K"
  given UnitName[Seconds[1]] = () => t"s"

  given UnitName[Radians[1]] = () => t"rad"

  given UnitName[Kilograms[1]] with
    override def siPrefix: MetricPrefix = Kilo
    def name(): Text = t"g"
