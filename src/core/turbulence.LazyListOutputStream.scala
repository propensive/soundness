/*
    Turbulence, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import language.experimental.captureChecking

import java.io as ji

import scala.collection.mutable as scm

import anticipation.*
import rudiments.*
import vacuous.*

class LazyListOutputStream() extends ji.OutputStream:
  private val buffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
  private val chunks: Spool[Bytes] = Spool()

  def stream: LazyList[Bytes] = chunks.stream
  def write(int: Int): Unit = buffer.append(int.toByte)

  override def close(): Unit = flush().also(chunks.stop())
  override def write(bytes: Array[Byte]): Unit = chunks.put(bytes.immutable(using Unsafe))

  override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
    chunks.put(bytes.slice(offset, offset + length).immutable(using Unsafe))

  override def flush(): Unit = if !buffer.isEmpty then
    chunks.put(buffer.toArray.immutable(using Unsafe))
    buffer.clear()
