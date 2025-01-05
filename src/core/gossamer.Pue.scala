/*
    Gossamer, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import rudiments.*

import language.experimental.pureFunctions
import language.experimental.into

object Pue:
  def apply(text: into Text): Bytes =
    val length = text.length

    IArray.create[Byte](length): array =>
      var i = 0
      while i < length do
        array(i) = (text.s.charAt(i)%0x100).toByte
        i += 1
