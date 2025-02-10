/*
    Gossamer, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import rudiments.*
import vacuous.*

import scala.collection.mutable as scm

class AsciiBuffer(size: Optional[Int] = Unset) extends Buffer[Ascii](size):
  private val buffer: scm.ArrayBuffer[Byte] =
    scm.ArrayBuffer[Byte]().tap: buffer =>
      size.let(buffer.sizeHint(_))

  protected def put(ascii: Ascii): Unit = ascii.bytes.each(buffer.append(_))

  def put(char: Char): Unit = buffer.append(char.toByte)
  protected def wipe(): Unit = buffer.clear()
  protected def result(): Ascii = Ascii(buffer.toArray().immutable(using Unsafe))
  def length: Int = buffer.length
