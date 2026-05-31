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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import anticipation.{Data as Bytes, *}
import contingency.*
import rudiments.*
import vacuous.*

import Http2.Frame
import Http2Error.Reason

// Pulls whole HTTP/2 frames from a byte stream that arrives in arbitrary chunks
// (a socket). Buffers leftover bytes between reads and blocks (by demanding the
// next chunk from the underlying iterator) until a full frame — its 9-byte header
// plus declared payload — is available. Returns `Unset` at end of stream.
class FrameReader(chunks: Iterator[Bytes]):
  private var buffer: Array[Byte] = new Array(0)
  private var pos: Int = 0

  // Ensure at least `n` unread bytes are buffered; false if the stream ends first.
  private def ensure(n: Int): Boolean =
    while buffer.length - pos < n && chunks.hasNext do
      val remaining = buffer.length - pos
      val chunk = chunks.next()
      val grown = new Array[Byte](remaining + chunk.length)
      System.arraycopy(buffer, pos, grown, 0, remaining)
      System.arraycopy(chunk.mutable(using Unsafe), 0, grown, remaining, chunk.length)
      buffer = grown
      pos = 0

    buffer.length - pos >= n

  private def slice(n: Int): Bytes =
    val out = new Array[Byte](n)
    System.arraycopy(buffer, pos, out, 0, n)
    pos += n
    out.immutable(using Unsafe)

  // Read the next frame, or `Unset` at clean end of stream.
  def next()(using Tactic[Http2Error]): Optional[Frame] =
    if !ensure(9) then Unset else
      val header = slice(9)
      val length = Frame.uint24(header, 0)
      if !ensure(length) then abort(Http2Error(Reason.Truncated))
      val whole = header ++ slice(length)
      Frame.decode(whole, 0)(0)
