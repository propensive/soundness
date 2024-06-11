/*
    Turbulence, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*

object LazyListInputStream:
  def apply(input: => LazyList[Bytes]): LazyListInputStream = new LazyListInputStream(input)

class LazyListInputStream(input: LazyList[Bytes]) extends ji.InputStream:
  private var stream: LazyList[Bytes] = input
  private var offset: Int = 0
  private var focus: Bytes = IArray.empty[Byte]

  override def available(): Int =
    val diff = focus.length - offset
    if diff > 0 then diff
    else if stream.isEmpty then 0
    else
      focus = stream.head
      stream = stream.tail
      offset = 0
      available()

  override def close(): Unit = ()

  def read(): Int = if available() == 0 then -1 else (focus(offset) & 0xff).also(offset += 1)

  override def read(array: Array[Byte], arrayOffset: Int, length: Int): Int =
    if length == 0 then 0 else
      val count = length.min(available())

      if count == 0 then -1 else
        if count > 0 then System.arraycopy(focus, offset, array, arrayOffset, count)
        offset += count
        count
