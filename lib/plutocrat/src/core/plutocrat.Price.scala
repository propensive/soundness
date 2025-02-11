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

import prepositional.*
import proscenium.*
import symbolism.*

case class Price[CurrencyType <: Currency & Singleton: ValueOf]
   (principal: Money[CurrencyType], tax: Money[CurrencyType]):

  def effectiveTaxRate: Double = tax/principal

  @targetName("add")
  infix def + (right: Price[CurrencyType]): Price[CurrencyType] =
    Price(principal + right.principal, tax + right.tax)

  @targetName("subtract")
  infix def - (right: Price[CurrencyType]): Price[CurrencyType] =
    Price(principal - right.principal, tax - right.tax)

  @targetName("negate")
  def `unary_-`: Price[CurrencyType] = Price(-principal, -tax)

  @targetName("divide")
  infix def / (right: Double): Price[CurrencyType] = Price(principal/right, tax/right)

  def inclusive: Money[CurrencyType] = principal + tax
