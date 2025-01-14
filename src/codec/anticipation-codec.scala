/*
    Anticipation, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import prepositional.*

type Bytes = IArray[Byte]

object Bytes:
  def apply(xs: Byte*): Bytes = IArray(xs*)
  def empty: Bytes = IArray()

  def construct(count: Int)(lambda: Array[Byte] => Unit): Bytes =
    val array: Array[Byte] = new Array[Byte](count)
    lambda(array)
    array.asInstanceOf[IArray[Byte]]

extension [ValueType: Encodable in Bytes](value: ValueType)
  def bytestream: Bytes = ValueType.encode(value)
 
extension (long: Long)
  def bytes: Bytes = IArray((56 to 0 by -8).map(long >> _).map(_.toByte)*)
