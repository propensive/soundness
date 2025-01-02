/*
    Gossamer, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

abstract class Buffer[TextType](size: Optional[Int] = Unset):
  protected def put(text: TextType): Unit
  protected def wipe(): Unit
  protected def result(): TextType

  def append(text: TextType): this.type = this.also(put(text))

  def build(block: this.type ?=> Unit): TextType =
    block(using this)
    apply()

  def apply(): TextType = result()
  def clear(): this.type = this.also(wipe())
  def empty: Boolean = length == 0
  def length: Int
