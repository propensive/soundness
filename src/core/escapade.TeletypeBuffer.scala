/*
    Escapade, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

import scala.collection.mutable as scm

class TeletypeBuffer(size: Optional[Int]) extends Buffer[Teletype]:
  private val buffer: StringBuilder = StringBuilder()
  private val spans: scm.Map[CharSpan, Ansi.Transform] = scm.HashMap()
  private val insertions: scm.Map[Int, Text] = scm.HashMap()
  private var offset: Int = 0

  def length: Int = buffer.length

  protected def wipe(): Unit =
    offset = 0
    buffer.clear()
    spans.clear()
    insertions.clear()

  protected def put(text: Teletype): Unit =
    buffer.append(text.plain.s)
    text.spans.each { (span, value) => spans(span.shift(offset)) = value }
    text.insertions.each { (position, value) => insertions(position + offset) = value }
    offset += text.length

  protected def result(): Teletype =
    Teletype(buffer.toString.tt, spans.to(TreeMap), insertions.to(TreeMap))
