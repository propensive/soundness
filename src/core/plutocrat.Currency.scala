/*
    Plutocrat, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package plutocrat

import anticipation.*

open case class Currency(isoCode: Text, symbol: Text, name: Text, modulus: Int):
  this: Currency =>
  def apply(value: Double): Money[this.type] =
    val integral = value.toLong
    val tweak = (if integral < 0 then -0.5 else 0.5)/modulus
    Money(this)(integral, ((value - integral + tweak)*modulus).toInt)

  def zero: Money[this.type] = apply(0.00)
