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
package susurration

import java.io as ji
import java.math as jm
import java.nio.charset as jnc

import scala.caps

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*

// The SSH wire encoding (RFC 4251 §5): bytes, booleans, big-endian `uint32`s and `uint64`s,
// length-prefixed `string`s (binary or UTF-8), two's-complement `mpint`s and comma-separated
// `name-list`s. Every message of the transport, authentication and connection protocols is a
// sequence of these, read with a `Wire.Reader` and written with a `Wire.Writer`.
object Wire:
  object Error:
    given communicable: Reason is Communicable =
      case Reason.Truncated         => m"the message ended before the field did"
      case Reason.Oversized(length) => m"a field claimed a length of $length bytes"
      case Reason.Trailing(count)   => m"$count bytes followed the end of the message"
      case Reason.Negative          => m"a negative integer was found where none is permitted"

    enum Reason(val number: Int) extends Clarification:
      case Truncated extends Reason(1)
      case Oversized(length: Long) extends Reason(2)
      case Trailing(count: Int) extends Reason(3)
      case Negative extends Reason(4)

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(599, reason.number)(m"could not decode the SSH message because $reason")

  // The largest field this reader accepts: SSH packets are at most 256 KiB in practice (RFC 4253
  // §6.1 requires 32 KiB), so a larger length is a corrupt or hostile message, not data to buffer.
  val Limit: Int = 1 << 24

  // A cursor over a message. Every read checks its bounds and reports a short or oversized
  // message through the tactic it takes, so the reader captures nothing; the frozen data is
  // indexed through its JVM array within those bounds.
  class Reader(data: Data) extends caps.Mutable:
    private val bytes: scala.Array[Byte] = Array.unsafeJvm(data)
    private var position: Int = 0

    def remaining: Int = bytes.length - position
    def finished: Boolean = position >= bytes.length

    private def need(count: Long)(using Tactic[Wire.Error], Diagnostics): Unit =
      if count < 0 || count > Limit then abort(Wire.Error(Error.Reason.Oversized(count)))
      if remaining < count then abort(Wire.Error(Error.Reason.Truncated))

    private update def next(): Int =
      val byte = bytes(position) & 0xff
      position += 1
      byte

    update def byte()(using Tactic[Wire.Error], Diagnostics): Byte =
      need(1)
      next().toByte

    update def boolean()(using Tactic[Wire.Error], Diagnostics): Boolean = byte() != 0

    update def uint32()(using Tactic[Wire.Error], Diagnostics): Int =
      need(4)
      (next() << 24) | (next() << 16) | (next() << 8) | next()

    update def uint64()(using Tactic[Wire.Error], Diagnostics): Long =
      need(8)
      (uint32().toLong << 32) | (uint32().toLong & 0xffffffffL)

    update def string()(using Tactic[Wire.Error], Diagnostics): Data =
      val length = uint32().toLong & 0xffffffffL
      need(length)
      take(length.toInt)

    // The next `count` bytes, unprefixed: a fixed-size field such as the KEX cookie.
    update def bytes(count: Int)(using Tactic[Wire.Error], Diagnostics): Data =
      need(count)
      take(count)

    update def text()(using Tactic[Wire.Error], Diagnostics): Text =
      String(Array.unsafeJvm(string()), jnc.StandardCharsets.UTF_8).tt

    update def mpint()(using Tactic[Wire.Error], Diagnostics): BigInt =
      val raw = Array.unsafeJvm(string())
      if raw.length == 0 then BigInt(0) else BigInt(jm.BigInteger(raw))

    update def names()(using Tactic[Wire.Error], Diagnostics): List[Text] =
      val list = text()
      if list.length == 0 then Nil else list.cut(t",")

    // The rest of the message, for a payload whose shape depends on what precedes it.
    update def rest(): Data = take(remaining)

    // Asserts that the message has been consumed entirely.
    def end()(using Tactic[Wire.Error], Diagnostics): Unit =
      if !finished then abort(Wire.Error(Error.Reason.Trailing(remaining)))

    private update def take(length: Int): Data =
      val output = Array.allocate[Byte](length)
      java.lang.System.arraycopy(bytes, position, output.raw, 0, length)
      position += length
      Array.freeze(output)

  // An accumulating message writer.
  class Writer() extends caps.Mutable:
    private val buffer: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()

    def length: Int = buffer.size

    update def byte(value: Byte): Unit = buffer.write(value.toInt)
    update def boolean(value: Boolean): Unit = buffer.write(if value then 1 else 0)

    update def uint32(value: Int): Unit =
      buffer.write(value >>> 24)
      buffer.write(value >>> 16)
      buffer.write(value >>> 8)
      buffer.write(value)

    update def uint64(value: Long): Unit =
      uint32((value >>> 32).toInt)
      uint32(value.toInt)

    update def string(data: Data): Unit =
      uint32(data.length)
      buffer.write(Array.unsafeJvm(data))

    // Unprefixed bytes: a fixed-size field, or a message already encoded.
    update def bytes(data: Data): Unit = buffer.write(Array.unsafeJvm(data))

    update def text(text: Text): Unit =
      // [java-boundary] fresh array returned by the JDK, retained by nobody else
      string(Array.unsafeFrozen(text.s.getBytes(jnc.StandardCharsets.UTF_8).nn))

    // An `mpint` is the shortest two's-complement encoding, which is what `toByteArray` gives;
    // zero is the empty string.
    update def mpint(value: BigInt): Unit =
      // [java-boundary] fresh array returned by the JDK, retained by nobody else
      if value == 0 then uint32(0) else string(Array.unsafeFrozen(value.toByteArray))

    update def names(names: List[Text]): Unit = text(names.join(t","))

    // [java-boundary] fresh array returned by the JDK, retained by nobody else
    def data: Data = Array.unsafeFrozen(buffer.toByteArray.nn)
