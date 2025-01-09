/*
    Spectacular, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectacular

import anticipation.*
import denominative.*
import rudiments.*
import vacuous.*

object Enumerable:
  inline given [EnumType <: reflect.Enum] => EnumType is Enumerable as derived =
    ${Spectacular.enumerable[EnumType]}

trait Enumerable:
  type Self <: reflect.Enum
  private lazy val valuesMap: Map[Text, Self] = values.indexBy(_.toString.tt)
  val name: Text
  val values: IArray[Self]
  def value(name: Text): Optional[Self] = valuesMap.at(name)
  def name(value: Self): Text = value.toString.tt
  def index(value: Self): Int = value.ordinal

  def value(ordinal: Ordinal): Optional[Self] =
    if ordinal.n0 >= 0 && ordinal.n0 < values.length then values(ordinal.n0) else Unset
