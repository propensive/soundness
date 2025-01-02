/*
    Gastronomy, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gastronomy

import scala.compiletime.*, ops.int.*

import prepositional.*
import rudiments.*

case class Digester(run: Digestion => Unit):
  def apply[HashType <: Algorithm](using function: HashFunction in HashType): Digest in HashType =
    function.init().pipe: accumulator =>
      run(accumulator)
      Digest[HashType](accumulator.digest())

  def digest[ValueType: Digestible](value: ValueType): Digester = Digester: accumulator =>
    run(accumulator)
    ValueType.digest(accumulator, value)
