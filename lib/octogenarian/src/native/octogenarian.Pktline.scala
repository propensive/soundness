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
package octogenarian

import scala.caps

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

// The pkt-line framing of git's wire protocols: each packet is four hexadecimal digits giving
// its whole length, then its bytes; the lengths 0, 1 and 2 are the flush, delimiter and
// response-end packets, which carry nothing.
private[octogenarian] object Pktline:
  import Git.Error.Reason as Reason

  enum Packet:
    case Line(data: Data)
    case Flush, Delim, End

    def text: Text = this match
      case Line(data) => Objects.text(data)
      case Flush      => t""
      case Delim      => t""
      case End        => t""

  private val truncated: Text = t"the stream ended mid-packet"

  def flush: Data = Objects.bytes(t"0000")
  def delim: Data = Objects.bytes(t"0001")
  def end: Data = Objects.bytes(t"0002")

  def line(text: Text): Data = frame(Objects.bytes(text))

  def frame(data: Data): Data =
    val length = data.length + 4
    val digits = String.format("%04x", Integer.valueOf(length)).nn.tt
    Store.concatenate(Objects.bytes(digits), data)

  // Pieces, in groups, concatenated into one request body.
  def concatenate(groups: List[List[Data]]): Data =
    val output = java.io.ByteArrayOutputStream()

    def write(parts: List[Data]): Unit = parts match
      case part :: rest => output.write(part.asInstanceOf[scala.Array[Byte]]); write(rest)
      case _            => ()

    groups.each(write(_))
    output.toByteArray.nn.asInstanceOf[Data]

  object Reader:
    // A reader over a pull endpoint, which it alone drains. A JDK stream cannot carry the
    // endpoint's capability, so the view is laundered pure at this one point.
    def from(consume stream: (Stream[Data] over Credit)^)(using Buffering): Reader =
      // [live-view] a pure-typed JDK view over the live endpoint, which this reader alone drains
      Reader(caps.unsafe.unsafeAssumePure(stream.inputStream))

  // Reads packets from a byte stream, blocking as the stream does.
  class Reader(input: java.io.InputStream):
    def underlying: java.io.InputStream = input

    // Exactly `count` bytes, or none if the stream ended first.
    def exactly(count: Int)(using Tactic[Git.Error]): Optional[Data] =
      // The buffer travels through the recursion rather than being captured by it.
      def recur(buffer: scala.Array[Byte]^, position: Int): Optional[Data] =
        if position >= count then buffer.asInstanceOf[Data] else
          val read = input.read(buffer, position, count - position)

          if read < 0 then
            if position == 0 then Unset else abort(Git.Error(Reason.Unexpected, truncated))
          else
            recur(buffer, position + read)

      recur(new scala.Array[Byte](count), 0)

    def next()(using Tactic[Git.Error]): Optional[Packet] = exactly(4).let(decode(_))

    private def decode(header: Data)(using Tactic[Git.Error]): Packet =
      val length = java.lang.Integer.parseInt(Objects.text(header).s, 16)

      if length == 0 then Packet.Flush
      else if length == 1 then Packet.Delim
      else if length == 2 then Packet.End
      else if length < 4 then abort(Git.Error(Reason.Unexpected, t"a packet of length $length"))
      else
        val body = exactly(length - 4).or(abort(Git.Error(Reason.Unexpected, truncated)))
        Packet.Line(body)

    // The lines up to the next flush (or delimiter, or the end of the stream), without it.
    def section()(using Tactic[Git.Error]): List[Packet.Line] =
      def recur(acc: List[Packet.Line]): List[Packet.Line] = next() match
        case Packet.Line(data) => recur(Packet.Line(data) :: acc)
        case _                 => acc.reverse

      recur(Nil)

    def close(): Unit = input.close()
