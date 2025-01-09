/*
    Vacuous, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import language.experimental.pureFunctions

import java.util as ju

import scala.quoted.*

import anticipation.*
import fulminate.*

import errorDiagnostics.stackTraces

object Unset:
  override def toString(): String = "∅"

type Optional[ValueType] = Unset.type | ValueType

extension [ValueType](inline optional: Optional[ValueType])
  inline def or(inline value: => ValueType): ValueType = ${Vacuous.optimizeOr('optional, 'value)}

extension [ValueType](optional: Optional[ValueType])
  inline def absent: Boolean = optional == Unset
  inline def present: Boolean = optional != Unset
  inline def vouch(using Unsafe): ValueType =
    optional.or(panic(m"a value was vouched but was absent"))

  inline def mask(predicate: ValueType => Boolean): Optional[ValueType] =
    optional.let { value => if predicate(value) then Unset else value }

  def stdlib: ju.Optional[ValueType] =
    optional.lay(ju.Optional.empty[ValueType].nn)(ju.Optional.of(_).nn)

  def presume(using default: Default[ValueType]): ValueType = optional.or(default())
  def option: Option[ValueType] = if absent then None else Some(vouch(using Unsafe))
  def assume(using absentValue: CanThrow[UnsetError]): ValueType = optional.or(throw UnsetError())

  inline def lay[ValueType2](inline alternative: => ValueType2)
     (inline lambda: ValueType => ValueType2)
          : ValueType2 =

    if absent then alternative else lambda(vouch(using Unsafe))


  inline def layGiven[ValueType2](inline alternative: => ValueType2)
     (inline block: ValueType ?=> ValueType2)
          : ValueType2 =

    if absent then alternative else block(using vouch(using Unsafe))

  def let[ValueType2](lambda: ValueType => ValueType2): Optional[ValueType2] =
    if absent then Unset else lambda(vouch(using Unsafe))

  inline def letGiven[ValueType2](inline block: ValueType ?=> ValueType2): Optional[ValueType2] =
    if absent then Unset else block(using vouch(using Unsafe))

object Optional:
  inline def apply[ValueType](value: ValueType | Null): Optional[ValueType] =
    if value == null then Unset else value
