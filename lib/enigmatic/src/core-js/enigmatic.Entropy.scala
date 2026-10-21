                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package enigmatic

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Uint8Array

import anticipation.*

// The one synchronous member of the Web Crypto API, `crypto.getRandomValues`, declared as a
// typed facade of the global `crypto` object (present in every browser and in Node from 19). The
// rest of Web Crypto is Promise-based and cannot back the synchronous `Crypto` contract; a
// provider over it waits on JSPI (roadmap plat-6).
@js.native
@JSGlobal("crypto")
private object WebCrypto extends js.Object:
  def getRandomValues(array: Uint8Array): Uint8Array = js.native

// Cryptographically strong random bytes for the pure-Scala provider, from `crypto.getRandomValues`,
// which fills at most 65536 bytes per call.
private[enigmatic] object Entropy:
  private val Chunk: Int = 65536

  def bytes(size: Int): Data =
    val output = Array.allocate[Byte](size)
    fill(output, 0, size)
    Array.freeze(output)

  // The exclusive array is a parameter, not a capture: separation checking forbids a local
  // function closing over a fresh exclusive array.
  private def fill(output: Array[Byte]^, offset: Int, size: Int): Unit =
    if offset < size then
      val count = Math.min(Chunk, size - offset)
      val array = new Uint8Array(count)
      WebCrypto.getRandomValues(array)
      copy(array, output, offset, 0, count)
      fill(output, offset + count, size)

  private def copy(array: Uint8Array, output: Array[Byte]^, offset: Int, index: Int, count: Int): Unit =
    if index < count then
      output(offset + index) = array(index).toByte
      copy(array, output, offset, index + 1, count)
