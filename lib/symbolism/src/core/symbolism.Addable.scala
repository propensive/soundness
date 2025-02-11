/*
    Symbolism, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package symbolism

import prepositional.*

import scala.annotation.targetName

object Addable:
  def apply[AugendType, AddendType, ResultType](lambda: (AugendType, AddendType) => ResultType)
  :     AugendType is Addable by AddendType into ResultType = new Addable:
    type Self = AugendType
    type Result = ResultType
    type Operand = AddendType

    def add(augend: AugendType, addend: AddendType): ResultType = lambda(augend, addend)

  given Double is Addable by Double into Double = Addable:
    (augend, addend) => augend + addend

  given Float is Addable by Float into Float = Addable:
    (augend, addend) => augend + addend

  given Long is Addable by Long into Long = Addable:
    (augend, addend) => augend + addend

  given Int is Addable by Int into Int = Addable:
    (augend, addend) => augend + addend

  given Short is Addable by Short into Short = Addable:
    (augend, addend) => (augend + addend).toShort

  given Byte is Addable by Byte into Byte = Addable:
    (augend, addend) => (augend + addend).toByte

trait Addable:
  type Self
  type Augend = Self
  type Addend = Operand
  type Result
  type Operand
  def add(augend: Augend, addend: Addend): Result

  extension (augend: Augend)
    @targetName("add")
    inline infix def + (addend: Addend): Result = add(augend, addend)
