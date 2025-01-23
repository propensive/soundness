/*
    Vacuous, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import java.util as ju

import scala.compiletime.*

import anticipation.*
import fulminate.*

inline def default[ValueType]: ValueType = summonInline[Default[ValueType]]()

inline def optimizable[ValueType](lambda: Optional[ValueType] => Optional[ValueType])
:     Optional[ValueType] =
  lambda(Unset)

extension [ValueType](iterable: Iterable[Optional[ValueType]])
  transparent inline def compact: Iterable[ValueType] =
    iterable.filter(!_.absent).map(_.vouch(using Unsafe))

extension [ValueType](option: Option[ValueType])
  inline def optional: Unset.type | ValueType = option.getOrElse(Unset)

extension [ValueType](value: ValueType)
  def puncture(point: ValueType): Optional[ValueType] = if value == point then Unset else value

  def only[ValueType2](partial: PartialFunction[ValueType, ValueType2]): Optional[ValueType2] =
    (partial.orElse { _ => Unset })(value)

  def unless(predicate: => Boolean) = if predicate then Unset else value
  def unless(predicate: ValueType => Boolean) = if predicate(value) then Unset else value

  def provided(predicate: => Boolean) = if predicate then value else Unset
  def provided(predicate: ValueType => Boolean) = if predicate(value) then value else Unset

extension [ValueType](java: ju.Optional[ValueType])
  def optional: Optional[ValueType] = if java.isEmpty then Unset else java.get.nn

given Realm = realm"vacuous"
