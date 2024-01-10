/*
    Vacuous, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package vacuous

import anticipation.*

import scala.compiletime.*

object Default:
  def apply[ValueType](value: => ValueType): Default[ValueType] = () => value
  given int: Default[Int] = () => 0
  given singleton[ValueType: ValueOf]: Default[ValueType] = () => valueOf[ValueType]
  given default: Default[Long] = () => 0L
  given text: Default[Text] = () => "".tt
  given string: Default[String] = () => ""
  given list[ElemType]: Default[List[ElemType]] = () => Nil
  given set[ElemType]: Default[Set[ElemType]] = () => Set()
  given vector[ElemType]: Default[Vector[ElemType]] = () => Vector()

trait Default[+ValueType]:
  def apply(): ValueType

inline def default[ValueType]: ValueType = summonInline[Default[ValueType]]()

