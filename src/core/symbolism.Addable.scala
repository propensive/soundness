/*
    Symbolism, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import language.experimental.captureChecking

import scala.annotation.targetName

object Addable:
  class Basic[AugendType, AddendType, ResultType]
      (lambda: (AugendType, AddendType) => ResultType)
  extends Addable[AddendType]:
    type Self = AugendType
    type Result = ResultType

    def add(augend: AugendType, addend: AddendType): ResultType = lambda(augend, addend)

  given Double is Addable[Double] into Double as double = _ + _
  given Float is Addable[Float] into Float as float = _ + _
  given Long is Addable[Long] into Long as long = _ + _
  given Int is Addable[Int] into Int as int = _ + _
  given Short is Addable[Short] into Short as short = (augend, addend) => (augend + addend).toShort
  given Byte is Addable[Byte] into Byte as byte = (augend, addend) => (augend + addend).toByte

trait Addable[-AddendType]:
  type Self
  type Augend = Self
  type Result
  def add(augend: Augend, addend: AddendType): Result

  extension (augend: Augend)
    @targetName("add")
    inline infix def + (addend: AddendType): Result = add(augend, addend)
