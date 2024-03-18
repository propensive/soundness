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
import fulminate.*

import scala.quoted.*

import language.experimental.pureFunctions

import _root_.java.util as ju

object Unset:
  override def toString(): String = "∅"

type Optional[ValueType] = Unset.type | ValueType

case class UnsetValueError() extends Error(Message("the value was not set".tt))

extension [ValueType](inline optional: Optional[ValueType])
  inline def or(inline value: => ValueType): ValueType = ${Vacuous.optimizeOr('optional, 'value)}

extension [ValueType](optional: Optional[ValueType])
  inline def absent: Boolean = optional == Unset
  inline def present: Boolean = optional != Unset
  inline def vouch(using Unsafe): ValueType =
    optional.or(throw Panic(msg"a value was vouched but was absent"))

  def stdlib: ju.Optional[ValueType] = optional.lay(ju.Optional.empty[ValueType].nn)(ju.Optional.of(_).nn)
  def presume(using default: Default[ValueType]): ValueType = optional.or(default())
  def option: Option[ValueType] = if absent then None else Some(vouch(using Unsafe))
  def assume(using absentValue: CanThrow[UnsetValueError]): ValueType = optional.or(throw UnsetValueError())

  inline def lay[ValueType2](inline alternative: => ValueType2)(inline lambda: ValueType => ValueType2)
          : ValueType2 =

    if absent then alternative else lambda(vouch(using Unsafe))


  inline def layGiven[ValueType2](inline alternative: => ValueType2)(inline block: ValueType ?=> ValueType2)
          : ValueType2 =

    if absent then alternative else block(using vouch(using Unsafe))


  inline def let[ValueType2](inline lambda: ValueType => ValueType2): Optional[ValueType2] =
    if absent then Unset else lambda(vouch(using Unsafe))
  
  inline def letGiven[ValueType2](inline block: ValueType ?=> ValueType2): Optional[ValueType2] =
    if absent then Unset else block(using vouch(using Unsafe))

extension [ValueType](iterable: Iterable[Optional[ValueType]])
  transparent inline def compact: Iterable[ValueType] = iterable.filter(!_.absent).map(_.vouch(using Unsafe))

object Optional:
  inline def apply[ValueType](value: ValueType | Null): Optional[ValueType] =
    if value == null then Unset else value

extension [ValueType](option: Option[ValueType])
  inline def optional: Unset.type | ValueType = option.getOrElse(Unset)
  def presume(using default: Default[ValueType]) = option.getOrElse(default())

extension [ValueType](value: ValueType)
  def puncture(point: ValueType): Optional[ValueType] = if value == point then Unset else value
  
  def only[ValueType2](partial: PartialFunction[ValueType, ValueType2]): Optional[ValueType2] =
    (partial.orElse { _ => Unset })(value)

extension [ValueType](java: ju.Optional[ValueType])
  def optional: Optional[ValueType] = if java.isEmpty then Unset else java.get.nn

erased trait Unsafe
erased val Unsafe: Unsafe = compiletime.erasedValue

trait Extractor[-ScrutineeType, +ExtractionType]:
  def extract(value: ScrutineeType): Optional[ExtractionType]
  def unapply(value: ScrutineeType): Option[ExtractionType] = extract(value).option