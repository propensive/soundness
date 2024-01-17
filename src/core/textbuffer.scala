/*
    Gossamer, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import anticipation.*

abstract class Buffer[TextType]():
  protected val textual: Textual[TextType]
  protected def append(text: TextType): Unit
  protected def wipe(): Unit
  protected def result(): TextType
  
  @targetName("append")
  def += [ValueType: textual.ShowType](value: ValueType): this.type =
    this.also(append(textual.show(value)))
  
  def apply(): TextType = result()
  def clear(): this.type = this.also(wipe())

case class TextBuffer() extends Buffer[Text]():
  private val buffer: StringBuilder = StringBuilder()
  protected val textual: Textual[Text] = Textual.text
  protected def append(text: Text): Unit = buffer.append(text)
  protected def wipe(): Unit = buffer.clear()
  protected def result(): Text = buffer.toString().tt