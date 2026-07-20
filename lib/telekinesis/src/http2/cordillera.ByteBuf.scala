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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package cordillera

import scala.caps

import anticipation.{Data as Bytes, *}
import prepositional.*
import vacuous.*
import rudiments.*

// A minimal growable byte buffer, classified as an exclusive, stateful capability:
// the separation checker rejects `scala.collection.mutable.ArrayBuilder` mutation
// outside a `uses`-clause scope, so the HTTP/2 wire encoders append through this
// instead. `data` copies out, so the internal storage never escapes.
private[cordillera] class ByteBuf(initial: Int = 32)
extends caps.ExclusiveCapability, caps.Stateful:
  // Untracked: reached only through this (exclusive) buffer, and `data` copies out.
  @caps.unsafe.untrackedCaptures
  private var storage: Array[Byte] = new Array[Byte](initial.max(8))
  private var size0: Int = 0

  // An exclusive view for writes: the untracked field reads as read-only.
  private inline def target: Array[Byte]^ = storage.asInstanceOf[Array[Byte]^]

  def size: Int = size0

  private update def ensure(extra: Int): Unit =
    if size0 + extra > storage.length then
      var capacity = storage.length*2
      while size0 + extra > capacity do capacity *= 2
      val grown = new Array[Byte](capacity)
      System.arraycopy(storage, 0, grown, 0, size0)
      // The cast erases the fresh array's capture: it is confined to this buffer.
      storage = grown.asInstanceOf[Array[Byte]]

  update def add(byte: Byte): Unit =
    ensure(1)
    target(size0) = byte
    size0 += 1

  update def addAll(bytes: Bytes): Unit =
    ensure(bytes.length)
    System.arraycopy(bytes.mutable(using Unsafe), 0, target, size0, bytes.length)
    size0 += bytes.length

  def data: Bytes =
    val out = new Array[Byte](size0)
    System.arraycopy(storage, 0, out, 0, size0)
    // Sealed: a fresh copy no alias can reach is immutable.
    caps.unsafe.unsafeAssumePure(out.immutable(using Unsafe))
