/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

case class UnsetValueError() extends Error(Message(Text("the value was not set")))

extension [ValueType](opt: Maybe[ValueType])
  def unset: Boolean = opt == Unset
  def cast(using Unsafe.type): ValueType = opt.asInstanceOf[ValueType]
  def or(value: => ValueType): ValueType^{value} = if unset then value else cast(using Unsafe)
  def presume(using default: => Default[ValueType]): ValueType^{default} = or(default())
  def avow: ValueType = or(throw Mistake("avowed value was unset"))
  
  def assume(using unsetValue: CanThrow[UnsetValueError]): ValueType^{unsetValue} =
    or(throw UnsetValueError())
  
  
  def option: Option[ValueType] = if unset then None else Some(cast(using Unsafe))

  def fm
      [ValueType2](default: => ValueType2)(fn: ValueType => ValueType2)
      : ValueType2^{default, fn} =
    if unset then default else fn(cast(using Unsafe))

  def mm[ValueType2](fn: ValueType => ValueType2): Maybe[ValueType2]^{fn} =
    if unset then Unset else fn(cast(using Unsafe))

extension [ValueType](opt: Option[ValueType])
  def maybe: Unset.type | ValueType = opt.getOrElse(Unset)
  def presume(using default: Default[ValueType]) = opt.getOrElse(default())
