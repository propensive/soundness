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
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*
import prepositional.*
import zephyrine.{Stream, Credit, Buffering, Substrate}

import Http2.Frame
import Http2Error.Reason

// Pulls whole HTTP/2 frames from a connection's pull endpoint, whose bytes arrive
// in arbitrary chunks (a socket). Buffers leftover bytes between reads and blocks
// (by refilling the endpoint) until a full frame — its 9-byte header plus declared
// payload — is available. Returns `Unset` at end of stream. The reader consumes the
// endpoint: it is the connection's single reader.
object FrameReader:
  // Only a method may declare `consume`: the factory takes ownership of the
  // endpoint and moves it into the reader as a neutral carrier.
  def apply(consume input: (Stream[Bytes] over Credit)^)(using Buffering): FrameReader^ =
    new FrameReader(input.asInstanceOf[AnyRef])

// An exclusive, stateful capability, like a parser: it owns the connection's read
// endpoint and its own reassembly buffer.
class FrameReader private (input0: AnyRef)(using buffering: Buffering)
extends caps.ExclusiveCapability, caps.Stateful:
  private inline def input: (Stream[Bytes] over Credit)^ =
    input0.asInstanceOf[(Stream[Bytes] over Credit)^]

  private val demand: Credit = Credit(buffering.capacity(Substrate.Bytes))

  // Untracked: the reassembly buffer is reached only through this (exclusive)
  // reader, and every `slice` copies out of it.
  @caps.unsafe.untrackedCaptures
  private var buffer: Array[Byte] = new Array(0)
  private var pos: Int = 0

  // Ensure at least `n` unread bytes are buffered; false if the stream ends first.
  private update def ensure(n: Int): Boolean =
    var ended = false

    while buffer.length - pos < n && !ended do input.refill(demand) match
      case count: Int =>
        if count > 0 then
          val remaining = buffer.length - pos
          val grown = new Array[Byte](remaining + count)
          System.arraycopy(buffer, pos, grown, 0, remaining)
          System.arraycopy(input.window(using Unsafe), input.start, grown, remaining, count)
          input.skip(count)
          // The cast erases the fresh array's capture: it is confined to this
          // (exclusive) reader from here on.
          buffer = grown.asInstanceOf[Array[Byte]]
          pos = 0

      case _ =>
        ended = true

    buffer.length - pos >= n

  private update def slice(n: Int): Bytes =
    val out = new Array[Byte](n)
    System.arraycopy(buffer, pos, out, 0, n)
    pos += n
    out.immutable(using Unsafe)

  // Consume and validate the given connection preface (RFC 7540 §3.5) ahead of
  // the first frame — the server role's first read on a new connection. A
  // mismatch (or a stream ending mid-preface) is a protocol error.
  update def expectPreface(preface: Bytes)(using Tactic[Http2Error]): Unit =
    if !ensure(preface.length)
    then abort(Http2Error(Reason.Protocol(t"bad connection preface")))

    val read: Bytes = slice(preface.length)
    var index: Int = 0

    while index < preface.length do
      if read(index) != preface(index)
      then abort(Http2Error(Reason.Protocol(t"bad connection preface")))
      index += 1

  // Read the next frame, or `Unset` at clean end of stream. The tactic is a plain
  // using-parameter: a context-function result may not hide `this`.
  update def next()(using Tactic[Http2Error]): Optional[Frame] =
    if !ensure(9) then Unset else
      val header = slice(9)
      val length = Frame.uint24(header, 0)
      if !ensure(length) then abort(Http2Error(Reason.Truncated))
      val whole: Bytes = header ++ slice(length)
      Frame.decode(whole, 0)(0)
