/*
    Rudiments, version 0.4.0. Copyright 2020-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import language.experimental.captureChecking

object Unset:
  override def toString(): String = "[unset]"

type Maybe[ValueType] = Unset.type | ValueType

case class UnsetValueError()
extends Error(ErrorMessage[EmptyTuple](List(Text("the value was not set")), EmptyTuple))

extension [ValueType](opt: Maybe[ValueType])
  def unset: Boolean = opt == Unset
  
  def or(value: {*}-> ValueType): {value} ValueType = opt match
    case Unset                       => value
    case other: ValueType @unchecked => other

  def presume(using default: Default[ValueType]): ValueType = or(default())
  def assume(using unsetValue: CanThrow[UnsetValueError]): {unsetValue} ValueType = or(throw UnsetValueError())

  def fm[ValueType2](default: -> ValueType2)(fn: ValueType -> ValueType2) = opt match
    case Unset                       => default
    case value: ValueType @unchecked => fn(value)

  def mm[ValueType2](fn: ValueType -> ValueType2): Maybe[ValueType2] = opt match
    case Unset                       => Unset
    case value: ValueType @unchecked => fn(value)

  def option: Option[ValueType] = opt match
    case Unset                       => None
    case other: ValueType @unchecked => Some(other)

extension [ValueType](opt: Option[ValueType])
  def maybe: Unset.type | ValueType = opt.getOrElse(Unset)
  def presume(using default: Default[ValueType]) = opt.getOrElse(default())
