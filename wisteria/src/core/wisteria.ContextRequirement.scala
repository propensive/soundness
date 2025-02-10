/*
    Wisteria, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria

import anticipation.*
import vacuous.*

import scala.compiletime.*

object ContextRequirement:
  given required: ContextRequirement:
    type Optionality[Type] = Type
    type Required = true
    def wrap[ValueType](optional: Optional[ValueType]): ValueType = optional.vouch

  object relaxed extends ContextRequirement:
    type Optionality[Type] = Optional[Type]
    type Required = false
    def wrap[ValueType](optional: Optional[ValueType]): Optional[ValueType] = optional

trait ContextRequirement:
  type Optionality[Type] <: Optional[Type]
  type Required <: Boolean
  def wrap[ValueType](optional: Optional[ValueType]): Optionality[ValueType]

  inline def summon[ContextualType]: Optionality[ContextualType] =
    inline if erasedValue[Required] then wrap(summonInline[ContextualType]) else summonFrom:
      case contextual: ContextualType => wrap(contextual)
      case _                          => wrap(Unset)
